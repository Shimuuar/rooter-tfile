import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.Get
import Data.Int
import Data.Word
import Text.Groom
----------------------------------------------------------------

type Offset = Int64

data Span = Span Offset Int32
          deriving Show

data RootHeader = RootHeader
  { rootVersion  :: Int32
  , rootBEGIN    :: Offset
  , rootEND      :: Offset
  , rootFree     :: Span   -- ^ FREE data record
  , rootNFree    :: Int32
  , rootNBytes   :: Word8
  , rootCompress :: Int32
  , rootSeekInfo :: Span
  , rootUUID     :: Word32
  }
  deriving Show


data RootObj = RootObj
  { objKeyVersion :: Int16
  , objRawSize    :: Int32      -- ^ Uncompressed size
  , objDataSize   :: Int32      -- ^ Compressed size
  , objDirectory  :: Offset
  , objDate       :: Int32
  , objCycle      :: Int16
  , objClass      :: String     -- ^
  , objName       :: String     -- ^
  , objTitle      :: String     -- ^
  }
  deriving (Show)

----------------------------------------------------------------

getRootHeader :: Get RootHeader
getRootHeader = do
  skip 4                        -- CHECK "root"
  v <- getInt32le
  let getPtr | v < 100000 = fromIntegral <$> getInt32le
             | otherwise  = getInt64le
  beg   <- fromIntegral <$> getInt32le
  end   <- getPtr
  free  <- getPtr
  nF    <- getInt32le
  nFree <- getInt32le
  skip 4
  un <- getWord8
  cmp <- getInt32le
  stream <- Span <$> getPtr <*> getInt32le
  uid <- get
  return $ RootHeader { rootVersion  = v
                      , rootBEGIN    = beg
                      , rootEND      = end
                      , rootFree     = Span free nF
                      , rootNFree    = nFree
                      , rootNBytes   = un
                      , rootCompress = cmp
                      , rootSeekInfo = stream
                      , rootUUID     = uid
                      }

getRootObj :: Int64 -> Get RootObj
getRootObj off0 = do
  size  <- getInt32le
  keyV  <- getInt16le
  sizeU <- getInt32le
  date  <- getInt32le
  keyL  <- getInt16le
  cycl  <- getInt16le
  --
  let getOff = fromIntegral <$> getInt32le
  off <- getOff
  dir <- getOff
  --
  let getStr = do n <- fromIntegral <$> getWord8
                  replicateM n (toEnum . fromIntegral <$> getWord8)
  classNm <- getStr
  objNm   <- getStr
  title   <- getStr
  return $ RootObj { objKeyVersion = keyV
                   , objRawSize    = size
                   , objDataSize   = sizeU
                   , objDirectory  = dir
                   , objDate       = date
                   , objCycle      = cycl
                   , objClass      = classNm
                   , objName       = objNm
                   , objTitle      = title
                   }


----------------------------------------------------------------


goObj bs off = do
  case runGet (getRootObj off) $ BS.drop (fromIntegral off) bs of
    Right x -> do putStrLn $ groom x
                  return x
    Left  e -> error e

go _  _   0 = return ()
go bs off n = do
  o1 <- goObj bs off
  go bs (off + fromIntegral (objRawSize o1)) (n - 1)
         
main = do
  bs <- BS.readFile "run-204-0020.root"
  let Right h = runGet getRootHeader bs
  putStrLn $ groom h
  --
  go bs (rootBEGIN h) 20

  -- o1 <- goObj bs off1
  -- let off2 = off1 + fromIntegral (objRawSize o1)
  -- o2 <- goObj bs off2
  -- --
  -- let off3 = off2 + fromIntegral (objRawSize o2)
  -- print off3
  -- o3 <- goObj bs off3
  -- return ()

getInt16le :: Get Int16
getInt16le = get

getInt32le :: Get Int32
getInt32le = get

getInt64le :: Get Int64
getInt64le = get

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Pretty

import Data.Serialize
import Data.Serialize.Get
import Data.Int
import Data.Word
import Text.Groom
import Text.Printf

import qualified Codec.Compression.Zlib as GZip (compress,decompress)
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
  { objRawSize    :: Int32      -- ^ Uncompressed size
  , objDataSize   :: Int32      -- ^ Compressed size
  , objKeyVersion :: Int16
  , objKeyLen     :: Int16
  , objOffset     :: Offset
  , objDirectory  :: Offset
  , objDate       :: Int32
  , objCycle      :: Int16
  , objClass      :: String     -- ^
  , objName       :: String     -- ^
  , objTitle      :: String     -- ^
  , objData       :: Span
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
  isolate (fromIntegral size - 4) $ do
    keyV  <- getInt16le
    sizeU <- getInt32le
    date  <- getInt32le
    keyL  <- getInt16le
    cycl  <- getInt16le
    --
    let getOff = fromIntegral <$> getInt32le
    off <- getOff
    when (off /= off0) $
      fail $ printf "Bad offset %i instead of %i" off off0
    dir <- getOff
    --
    let getStr = do n <- fromIntegral <$> getWord8
                    replicateM n (toEnum . fromIntegral <$> getWord8)
    classNm  <- getStr
    objNm    <- getStr
    title    <- getStr
    dataSize <- remaining
    getByteString dataSize
    return $ RootObj { objRawSize    = size
                     , objDataSize   = sizeU
                     , objKeyVersion = keyV
                     , objKeyLen     = keyL
                     , objOffset     = off
                     , objDirectory  = dir
                     , objDate       = date
                     , objCycle      = cycl
                     , objClass      = classNm
                     , objName       = objNm
                     , objTitle      = title
                     , objData       = Span (off + fromIntegral size - fromIntegral dataSize)
                                            (fromIntegral dataSize)
                     }


----------------------------------------------------------------
getData :: Span -> BS.ByteString -> BS.ByteString
getData (Span off n) = BS.take (fromIntegral n) . BS.drop (fromIntegral off)

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
  putStrLn "== HEADER ================"
  putStrLn $ groom h
  --
  putStrLn "-- Seek Info ----------------"
  seekI <- goObj bs (case rootSeekInfo h of Span o _ -> o)
  putStrLn ""
  let o   = getData (objData seekI) bs
      raw = decompress $ BS.drop 9 o
  print $ HexPretty $ raw
  --
  -- Data compression:
  --  * ZLib method
  --     0-1 :  ZL
  --     2   :  method
  --     3-5 :  compressed size
  --     6-9 :  uncompressed size


  -- go bs (rootBEGIN h) 10

  -- o1 <- goObj bs off1
  -- let off2 = off1 + fromIntegral (objRawSize o1)
  -- o2 <- goObj bs off2
  -- --
  -- let off3 = off2 + fromIntegral (objRawSize o2)
  -- print off3
  -- o3 <- goObj bs off3
  return ()

getInt16le :: Get Int16
getInt16le = get

getInt32le :: Get Int32
getInt32le = get

getInt64le :: Get Int64
getInt64le = get


-- Helper for gzip
withLazyBS :: (L.ByteString -> L.ByteString) -> BS.ByteString -> BS.ByteString
withLazyBS f = BS.concat . L.toChunks . f . L.fromChunks . pure

doDecode :: Serialize a => BS.ByteString -> a
doDecode bs =
  case decode bs of
    Left  err -> error $ "Undecodable data: " ++ err
    Right x   -> x

compress :: BS.ByteString -> BS.ByteString
compress = withLazyBS GZip.compress

decompress :: BS.ByteString -> BS.ByteString
decompress = withLazyBS GZip.decompress

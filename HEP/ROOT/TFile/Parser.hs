-- |
-- Parser for .root data files.
module HEP.ROOT.TFile.Parser (
    getRootHeader
  , getRootObj
  , getObjectMap
  ) where

import Control.Applicative
import Control.Monad

import Data.Int
import Data.Serialize
import qualified Data.ByteString      as BS

import Text.Printf

import HEP.ROOT.TFile.Types


----------------------------------------------------------------
-- Parsers
----------------------------------------------------------------

-- | Parse ROOT header
getRootHeader :: Get RootHeader
getRootHeader = do
  skip 4                        -- CHECK "root"
  v <- getInt32le
  let getPtr | v < 100000 = fromIntegral <$> getInt32le
             | otherwise  = getInt64le
  beg    <- fromIntegral <$> getInt32le
  end    <- getPtr
  free   <- getPtr
  nF     <- getInt32le
  nFree  <- getInt32le
  skip 4               -- Number of bytes in TNamed at creation time
  un     <- getWord8
  cmp    <- getInt32le
  stream <- Span <$> getPtr <*> getInt32le
  uid    <- get
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

-- | Get ROOT object. This parser doesn't consume data payload.
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
    when (off /= off0  &&  off /= 0) $
      fail $ printf "Bad offset %i instead of %i" off off0
    dir <- getOff
    --
    let getStr = do n <- fromIntegral <$> getWord8
                    replicateM n (toEnum . fromIntegral <$> getWord8)
    classNm  <- getStr
    objNm    <- getStr
    title    <- getStr
    dataSize <- remaining
    skip dataSize
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

-- | Get map of all objects contained in the file
getObjectMap :: BS.ByteString -> (RootHeader, [RootObj])
getObjectMap bs
  = (header, loop (rootBEGIN header))
  where
    -- Decode header
    header =
      case runGet getRootHeader bs of
        Right x -> x
        Left  e -> error e
    -- Decode all objects in the file
    loop off | off >= rootEND header = []
    loop off =
      case runGet (getRootObj off) $ BS.drop (fromIntegral off) bs of
        Right o -> o : loop (off + fromIntegral (objRawSize o))
        Left  e -> error e


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16be

getInt32le :: Get Int32
getInt32le =  fromIntegral <$> getWord32be

getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64be

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
import HEP.ROOT.TFile.Get


----------------------------------------------------------------
-- Parsers
----------------------------------------------------------------

-- | Parse ROOT header
getRootHeader :: Get RootHeader
getRootHeader = do
  skip 4                        -- CHECK "root"
  v <- getInt32be
  let getPtr | v < 100000 = fromIntegral <$> getInt32be
             | otherwise  = getInt64be
  beg    <- fromIntegral <$> getInt32be
  end    <- getPtr
  free   <- getPtr
  nF     <- getInt32be
  nFree  <- getInt32be
  skip 4               -- Number of bytes in TNamed at creation time
  un     <- getWord8
  cmp    <- getInt32be
  stream <- Span <$> getPtr <*> getInt32be
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
  size  <- getInt32be
  isolate (fromIntegral size - 4) $ do
    keyV  <- getInt16be
    sizeU <- getInt32be
    date  <- getInt32be
    keyL  <- getInt16be
    cycl  <- getInt16be
    --
    let getOff = fromIntegral <$> getInt32be
    off <- getOff
    when (off /= off0  &&  off /= 0) $
      fail $ printf "Bad offset %i instead of %i" off off0
    dir <- getOff
    --
    classNm  <- getRootString
    objNm    <- getRootString
    title    <- getRootString
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

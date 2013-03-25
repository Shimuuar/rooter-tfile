{-# LANGUAGE OverloadedStrings #-}
-- |
-- Parsers for ROOT objects.
--
module HEP.ROOT.TFile.Object (
    -- * Get serialized data
    getObjectData
    -- * Deserialize objects
    -- ** TObject
  , getTObject
  , getTNamed
  , getTH1I
    -- * Helpers
  , skipRootObject
  ) where
-- Data format for every class is described in the `Streamer' method.

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Codec.Compression.Zlib as GZip (compress,decompress)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy as L

import HEP.ROOT.TFile.Types
import HEP.ROOT.TFile.Get
import Debug.Trace

----------------------------------------------------------------
-- Extract data
----------------------------------------------------------------

-- | Get serialized object data.
getObjectData :: BS.ByteString -> RootObj -> BS.ByteString
-- Object data could be compressed or uncompressed. It seems that data
-- is assumed to be uncompressed when compressed size is equal to
-- uncompressed size.
--
-- Compressed data have header which contain compression method.
--
--  0-1 : compression type
--     ZL - zlib
--     XZ - LZMA
--     CS - old zlib
--  2   :  method
--  3-5 :  compressed size
--  6-9 :  uncompressed size
--
-- Source: root/core/zip/src/ZInflate.c: R__unzip()
getObjectData bs o
  -- Uncompressed data
  | objDataSize o == fromIntegral n = raw
  -- Data is compressed.
  | n < 9      = error "rooter.TFile: bad header for object."
  | ty == "ZL" = decompress $ BS.drop 9 raw
  | ty == "XZ" = error "rooter.TFile: LZMA compression is not supported"
  | ty == "CS" = error "rooter.TFile: old zlib method is not supported"
  | otherwise  = error "rooter.TFile: unknown object header"
  where
    ty  = BS.take 2 raw
    raw = getData (objData o) bs
    n   = BS.length raw

-- Slice of the data
getData :: Span -> BS.ByteString -> BS.ByteString
getData (Span off n)
  = BS.take (fromIntegral n) . BS.drop (fromIntegral off)


----------------------------------------------------------------
-- Parsers for objects
----------------------------------------------------------------

-- | Deserialize TObject. No useful data is retained.
getTObject :: Get ()
getTObject = do
  void getVersion
  -- We need to skip 2 additional bytes. I couldn't figure where they
  -- are consumed.
  skip 2
  -- Skip unique ID and bit mask
  skip 8

-- | Parse TNamed
getTNamed :: Get (String,String)
getTNamed = do
  getTObject
  (,) <$> getRootString <*> getRootString

-- getTH1I :: Get ()
getTH1I = do
  void getVersion
  void getVersion
  nm1 <- getTNamed
  skipRootObject                -- TAttLine
  skipRootObject                -- TAttFill
  skipRootObject                -- TAttMarker
  nCell <- getInt32be           --
  -- X axis
  v   <- getVersion
  nmX <- getTNamed
  skipRootObject                -- TAttAxis
  nBins <- getInt32be
  xMin <- getFloat32be
  xMax <- getFloat32be
  return (nCell,nm1,nmX,v,nBins,xMin,xMax)
  
readVersion :: Get Int16
readVersion = do
  n <- getVersion
  case n of
    0 -> fail "Cannot handle version 0"
    1 -> return 1
    _ -> fail "Cannot handle version above 1"

getVersion :: Get Int16
getVersion = do
  ver <- getInt16be
  if (ver .&. kByteCountMask /= 0)
    then skip 2 >> getInt16be
    else return ver

-- | Skip ROOT object without actually decoding it
skipRootObject :: Get ()
skipRootObject = do
  n <- getInt16be
  unless (n .&. kByteCountMask /= 0)
    $ fail "No byte length mask"
  skip . fromIntegral =<< getInt16be


kByteCountMask :: Int16
kByteCountMask = 0x4000

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

withLazyBS :: (L.ByteString -> L.ByteString) -> BS.ByteString -> BS.ByteString
withLazyBS f = BS.concat . L.toChunks . f . L.fromChunks . pure

compress :: BS.ByteString -> BS.ByteString
compress = withLazyBS GZip.compress

decompress :: BS.ByteString -> BS.ByteString
decompress = withLazyBS GZip.decompress

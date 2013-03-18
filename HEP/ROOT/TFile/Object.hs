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
  ) where
-- Data format for every class is described in the `Streamer' method.

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Serialize.Get
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
  ver <- getInt16be
  when (ver .&. kByteCountMask /= 0) $ do
    skip 4
  -- We need to skip 2 additional bytes. I couldn't figure where they
  -- are consumed.
  skip 2
  -- Skip unique ID and bit mask
  skip 8
  where
    kByteCountMask = 0x4000

-- | Parse TNamed
getTNamed :: Get (String,String)
getTNamed = do
  getTObject
  (,) <$> getRootString <*> getRootString


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

withLazyBS :: (L.ByteString -> L.ByteString) -> BS.ByteString -> BS.ByteString
withLazyBS f = BS.concat . L.toChunks . f . L.fromChunks . pure

compress :: BS.ByteString -> BS.ByteString
compress = withLazyBS GZip.compress

decompress :: BS.ByteString -> BS.ByteString
decompress = withLazyBS GZip.decompress

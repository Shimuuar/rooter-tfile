{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as HM
import Data.ByteString.Pretty

import Data.Serialize
import Data.Serialize.Get
import Data.Int
import Data.Word
import Text.Groom
import Text.Printf

import qualified Codec.Compression.Zlib as GZip (compress,decompress)

import HEP.ROOT.TFile.Types
import HEP.ROOT.TFile.Parser

----------------------------------------------------------------
-- Layout of data header
--  0-1 : compression type
--     ZL - zlib
--     XZ - LZMA
--     CS - old zlib
--  2   :  method
--  3-5 :  compressed size
--  6-9 :  uncompressed size
--
-- Source: root/core/zip/src/ZInflate.c: R__unzip()
--
-- They how classes are deserialized is described in function
-- `Streamer'

-- Slice of the data
getData :: Span -> BS.ByteString -> BS.ByteString
getData (Span off n)
  = BS.take (fromIntegral n) . BS.drop (fromIntegral off)

-- Get raw uncompressed data
getObjectData :: BS.ByteString -> RootObj -> BS.ByteString
getObjectData bs o
  | objDataSize o == fromIntegral n = raw
  -- Data is compressed.
  | n < 9                 = error "BAD header"
  | BS.take 2 raw == "ZL" = decompress $ BS.drop 9 raw
  | otherwise             = error "Something went wrong"
  where
    raw = getData (objData o) bs
    n   = BS.length raw

dumpObject :: BS.ByteString -> RootObj -> IO ()
dumpObject bs obj = do
  putStrLn "================================================================"
  putStrLn $ groom obj
  print $ HexPretty $ getObjectData bs obj


main :: IO ()
main = do
  bs <- BS.readFile "tst.root"
  let (h,objs) = getObjectMap bs
  --
  putStrLn "== HEADER ================"
  putStrLn $ groom h
  --
  putStrLn "== OBJS  ================"
  mapM_ (putStrLn . groom) objs
  --
  mapM_ (dumpObject bs) $ filter ((=="TNamed") . objClass) objs
  --
  return ()

----------------------------------------------------------------
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

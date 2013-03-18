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
import HEP.ROOT.TFile.Object

dumpObject :: BS.ByteString -> RootObj -> IO ()
dumpObject bs obj = do
  putStrLn "================================================================"
  putStrLn $ groom obj
  putStrLn "-----"
  let raw = getObjectData bs obj
  print $ HexPretty raw
  putStrLn "-----"
  print $ runGet getTNamed raw


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
  mapM_ (dumpObject bs) $ filter ((=="TH1I") . objClass) objs
  --
  return ()

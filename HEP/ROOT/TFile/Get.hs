-- |
-- Extra parsers
module HEP.ROOT.TFile.Get (
    -- * Fixed size integers
    getInt8
  , getInt16le
  , getInt32le
  , getInt64le    
  , getInt16be
  , getInt32be
  , getInt64be    
    -- * String
  , getRootString
  ) where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Serialize.Get


----------------------------------------------------------------
-- Integers
----------------------------------------------------------------

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

getInt32le :: Get Int32
getInt32le =  fromIntegral <$> getWord32le

getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64le


getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be =  fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be


----------------------------------------------------------------
-- ROOT primitives
----------------------------------------------------------------

getRootString :: Get String
getRootString = do
  n <- fromIntegral <$> getWord8
  replicateM n (toEnum . fromIntegral <$> getWord8)

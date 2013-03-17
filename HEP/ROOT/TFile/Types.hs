-- |
-- Data types for describing .root files.
module HEP.ROOT.TFile.Types (
    RootHeader(..)
  , RootObj(..)
  , Offset
  , Span(..)
  ) where

import Data.Int
import Data.Word

-- | Offset in the file.
type Offset = Int64

-- | Data range in the file.
data Span = Span Offset Int32
          deriving Show

-- | Header of the ROOT file.
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

-- | Description of object in the ROOT file
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

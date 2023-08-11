module RTF.Type (
  Word8,
  RTFHeader (..),
  ColorDef (..),
  RTFEncoding (..),
  FontFamily (..),
  FontInfo (..),
) where

import Data.Attoparsec.ByteString.Char8
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Word

data RTFHeader
  = FontTbl (NonEmpty FontInfo)
  | ColorTbl [ColorDef]
  deriving stock (Eq, Show)

data ColorDef = ColorDef {red :: Maybe Word8, green :: Maybe Word8, blue :: Maybe Word8}
  deriving stock (Eq, Show)

data FontInfo = FontInfo
  { fontNum :: Int
  , fontFamily :: FontFamily
  , fontName :: Text
  }
  deriving stock (Eq, Show)

data FontFamily = FNil | FRoman | FSwiss | FModern | FScript | FDecor | FTech | FBidi
  deriving stock (Eq, Show, Enum, Bounded)

class RTFEncoding c where
  encodeRTF :: c -> Text
  decodeRTF :: Parser c

module RTF.Types (
  RTFHeader (..),
  ColorTbl (..),
  FontTbl (..),
  RTFColor (..),
  FontFamily (..),
  FontInfo (..),
  Charset (..),
  Word8,
  Text,
  Parser,
  -- Lens
  _rtfCharset,
  _rtfCocoaControls,
  _rtfFontTbl,
  _rtfColors,
  _fontNum,
  _fontFamily,
  _fontCharset,
  _fontName,
  _FNil,
  _FRoman,
) where

import Data.Attoparsec.ByteString.Char8
import Data.Word
import RTF.ExtensionTypes
import Utils

-- \rtf <charset> \deff? <fonttbl> <filetbl>? <colortbl>? <stylesheet>? <listtables>? <revtbl>?
data RTFHeader = RTFHeader
  { rtfCharset :: Charset
  , rtfCocoaControls :: [CocoaControl]
  , rtfFontTbl :: FontTbl
  , rtfColors :: [(RTFColor, Maybe ColorSpace)]
  }
  deriving stock (Eq, Show)

newtype FontTbl = FontTbl [Maybe FontInfo]
  deriving stock (Eq, Show)

newtype ColorTbl = ColorTbl [RTFColor]
  deriving stock (Eq, Show)

data RTFColor = RTFColor {red :: Maybe Word8, green :: Maybe Word8, blue :: Maybe Word8}
  deriving stock (Eq, Show)

data FontInfo = FontInfo
  { fontNum :: Int
  , fontFamily :: FontFamily
  , fontCharset :: Maybe Int
  , fontName :: Text
  }
  deriving stock (Eq, Show)

data FontFamily = FNil | FRoman | FSwiss | FModern | FScript | FDecor | FTech | FBidi
  deriving stock (Eq, Show, Enum, Bounded)

newtype Charset = Ansi Int
  deriving stock (Eq, Show)

$(makeLensesWith dataLensRules ''FontInfo)
$(makeLensesWith dataLensRules ''RTFHeader)
$(makePrisms ''FontFamily)

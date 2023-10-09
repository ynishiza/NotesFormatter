module Notes.RTFDoc.Types (
  RTFHeader (..),
  ColorTbl (..),
  FontTbl (..),
  RTFColor (..),
  FontFamily (..),
  FontInfo (..),
  Charset (..),
  RTFDoc (..),
  allFontFamilies,
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
  _rtfDocContent,
  _rtfDocHeader,
  _FontTbl,
  _ColorTbl,
  _ExpandedColorTbl,
  _RTFDoc,
  _RTFHeader,
  --
  module X,
) where

import Data.Word
import Notes.RTF.Types as X
import Notes.RTFDoc.ExtensionTypes as X
import Notes.Utils as X

data RTFDoc = RTFDoc
  { rtfDocHeader :: RTFHeader
  , rtfDocContent :: [RTFElement]
  }
  deriving stock (Eq, Show, Generic)

-- \rtf <charset> \deff? <fonttbl> <filetbl>? <colortbl>? <stylesheet>? <listtables>? <revtbl>?
data RTFHeader = RTFHeader
  { rtfCharset :: Charset
  , rtfCocoaControls :: [CocoaControl]
  , rtfFontTbl :: FontTbl
  , rtfColors :: [(RTFColor, Maybe ColorSpace)]
  }
  deriving stock (Eq, Show, Generic)

newtype FontTbl = FontTbl [Maybe FontInfo]
  deriving stock (Eq, Show, Generic)

newtype ColorTbl = ColorTbl [RTFColor]
  deriving stock (Eq, Show, Generic)

data RTFColor = RTFColor {red :: Maybe Word8, green :: Maybe Word8, blue :: Maybe Word8}
  deriving stock (Eq, Show, Generic)

data FontInfo = FontInfo
  { fontNum :: Int
  , fontFamily :: FontFamily
  , fontCharset :: Maybe Int
  , fontName :: Text
  }
  deriving stock (Eq, Show, Generic)

data FontFamily = FNil | FRoman | FSwiss | FModern | FScript | FDecor | FTech | FBidi
  deriving stock (Eq, Show, Enum, Bounded, Generic)

allFontFamilies :: [FontFamily]
allFontFamilies = [minBound .. maxBound]

newtype Charset = Ansi Int
  deriving stock (Eq, Show, Generic)

$(makeLensesWith dataLensRules ''FontInfo)
$(makeLensesWith dataLensRules ''RTFHeader)
$(makeLensesWith dataLensRules ''RTFDoc)
$(makePrisms ''RTFDoc)
$(makePrisms ''RTFHeader)
$(makePrisms ''FontFamily)
$(makePrisms ''FontTbl)
$(makePrisms ''ColorTbl)
$(makePrisms ''ExpandedColorTbl)

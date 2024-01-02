module Notes.RTFDoc.Types (
  RTFHeader (..),
  ColorTbl (..),
  FontTbl (..),
  RTFColor (..),
  FontFamily (..),
  FontInfo (..),
  Charset (..),
  RTFDoc (..),
  RTFDocContent (..),
  rtfElementToDocContent,
  rtfElementToDocContent_,
  escapedSequenceChar,
  escapedSequenceRenderHex,
  escapedSequenceReadHex,
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
  _ContentControlSymbol,
  _ContentControlWord,
  _ContentGroup,
  _ContentText,
  _ContentEscapedSequence,
  --
  module X,
) where

import Data.Char (toLower)
import Data.Text qualified as T
import Data.Word
import Notes.RTF.Types as X
import Notes.RTFDoc.ExtensionTypes as X
import Notes.Utils as X
import Numeric (readHex)
import Text.Printf (printf)

data RTFDoc = RTFDoc
  { rtfDocHeader :: RTFHeader
  , rtfDocContent :: [RTFDocContent]
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

data RTFDocContent
  = ContentControlSymbol Char
  | ContentControlWord RTFControlPrefix Text RTFControlSuffix
  | ContentGroup [RTFDocContent]
  | ContentText Text
  -- | Escaped hex sequence
  --
  -- e.g. if fcharset128 (Shift JS)
  --
  --   \'82\'a0      あ  
  --
  | ContentEscapedSequence Word8
  deriving stock (Eq, Show, Generic)

escapedSequenceChar :: Char
escapedSequenceChar = '\''

escapedSequenceRenderHex :: Word8 -> String
escapedSequenceRenderHex = (toLower <$>) . printf "%02x"

escapedSequenceReadHex :: Text -> Maybe (Word8, Text)
escapedSequenceReadHex text =
  if T.length text >= 2
    then case readHex @Word8 (T.unpack hexText) of
      [(n, "")] -> Just (n, rest)
      _ -> Nothing
    else -- case: too short. Must be exactly two digits
      Nothing
 where
  (hexText, rest) = T.splitAt 2 text

rtfElementToDocContent :: RTFElement -> RTFDocContent
rtfElementToDocContent = rtfElementToDocContent_ rtfElementToDocContent

rtfElementToDocContent_ :: (RTFElement -> RTFDocContent) -> RTFElement -> RTFDocContent
rtfElementToDocContent_ _ (RTFText t) = ContentText t
rtfElementToDocContent_ _ (RTFControlSymbol c) = ContentControlSymbol c
rtfElementToDocContent_ _ (RTFControlWord prefix name suffix) = ContentControlWord prefix name suffix
rtfElementToDocContent_ f (RTFGroup x) = ContentGroup $ f <$> x

$(makeLensesWith dataLensRules ''FontInfo)
$(makeLensesWith dataLensRules ''RTFHeader)
$(makeLensesWith dataLensRules ''RTFDoc)
$(makePrisms ''RTFDoc)
$(makePrisms ''RTFHeader)
$(makePrisms ''FontFamily)
$(makePrisms ''FontTbl)
$(makePrisms ''ColorTbl)
$(makePrisms ''ExpandedColorTbl)
$(makePrisms ''RTFDocContent)

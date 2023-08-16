module RTF.Types (
  RTFGroup(..),
  RTFControlWord(..),
  RTFControlSymbol(..),
  RTFText(..),
  _NoTrailing,
  _RTFControlParam,
  _TrailingSpace,
  --
  RTFHeader (..),
  ColorTbl (..),
  FontTbl (..),
  RTFColor (..),
  FontFamily (..),
  FontInfo (..),
  Charset (..),
  RTFDoc (..),
  RTFContent (..),
  Word8,
  Text,
  Parser,
  RTFControlWordEnd (..),
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
  _RTFPlainText,
  _RTFLiteralSlash,
  _RTFLiteralOpenBrace,
  _RTFLiteralCloseBrace,
  _RTFNewLine,
  _RTFTag,
  _RTFBlock,
  --
) where

import Data.Attoparsec.ByteString.Char8
import Data.Word
import RTF.ExtensionTypes
import Utils

data RTFControlWord = RTFControlWord Text RTFControlWordEnd
  deriving stock (Eq, Show, Generic)

data RTFControlWordEnd = RTFControlParam Int | TrailingSpace | NoTrailing
  deriving stock (Eq, Show, Generic)

newtype RTFControlSymbol = RTFControlSymbol Char
  deriving stock (Eq, Show, Generic)

newtype RTFGroup a = RTFGroup a
  deriving stock (Eq, Show, Generic)

newtype RTFText = RTFText Text
  deriving stock (Eq, Show, Generic)

data RTFDoc = RTFDoc
  { rtfDocHeader :: RTFHeader
  , rtfDocContent :: [RTFContent]
  }
  deriving stock (Eq, Show, Generic)

data RTFContent
  = RTFLiteralSlash
  | RTFLiteralOpenBrace
  | RTFLiteralCloseBrace
  | RTFNewLine
  | RTFTag Text RTFControlWordEnd
  | RTFBlock Text
  | RTFPlainText Text
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

newtype Charset = Ansi Int
  deriving stock (Eq, Show, Generic)

$(makeLensesWith dataLensRules ''FontInfo)
$(makeLensesWith dataLensRules ''RTFHeader)
$(makeLensesWith dataLensRules ''RTFDoc)
$(makePrisms ''FontFamily)
$(makePrisms ''RTFContent)
$(makePrisms ''RTFControlWordEnd)

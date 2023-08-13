module RTF.Types (
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
  RTFTagEnd (..),
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
  _RTFText,
  _RTFLiteralSlash,
  _RTFLiteralOpenBrace,
  _RTFLiteralCloseBrace,
  _RTFNewLine,
  _RTFTag,
  _RTFBlock,
  _TrailingSymbol,
  _TagParameter,
  _TrailingSpace,
) where

import Data.Attoparsec.ByteString.Char8
import Data.Word
import RTF.ExtensionTypes
import Utils

data RTFDoc = RTFDoc
  { rtfDocHeader :: RTFHeader
  , rtfDocContent :: [RTFContent]
  }
  deriving stock (Eq, Show)

data RTFContent
  = RTFLiteralSlash
  | RTFLiteralOpenBrace
  | RTFLiteralCloseBrace
  | RTFNewLine
  | RTFTag Text RTFTagEnd
  | RTFBlock Text
  | RTFText Text
  deriving stock (Eq, Show)

data RTFTagEnd = TagParameter Word8 | TrailingSpace | TrailingSymbol
  deriving stock (Eq, Show)

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
$(makeLensesWith dataLensRules ''RTFDoc)
$(makePrisms ''FontFamily)
$(makePrisms ''RTFContent)
$(makePrisms ''RTFTagEnd)

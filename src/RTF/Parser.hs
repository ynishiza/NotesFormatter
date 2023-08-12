{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTF.Parser (
  anyRtfKeyword,
  charBlockEnd,
  module X,
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.Text qualified as PT

-- import Data.ByteString.Char8 (ByteString)
-- import Data.ByteString.Char8 qualified as B
-- import Data.Maybe (fromMaybe)

import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Exts (fromList)
import RTF.Type as X
import TextShow
import Prelude hiding (takeWhile)

instance RTFEncoding RTFHeader where
  encodeRTF (ColorTbl defs) =
    toRTFBlock $
      toRTFKeyword "colortbl" <> T.intercalate "" (encodeRTF <$> defs)
  encodeRTF (FontTbl infos) =
    toRTFBlock $
      toRTFKeyword "fonttbl" <> T.intercalate "" (encodeRTF <$> infos)

  decodeRTF = fontTbl <|> colorTbl
   where
    colorTbl = fromRTFBlock (fromRTFKeywordWith "colortbl" >> colorContent)
     where
      colorContent =
        ColorTbl <$> many' (skipSpace *> decodeRTF @ColorDef <* skipSpace)
          <?> "ColorTbl"

    fontTbl = fromRTFBlock (fromRTFKeywordWith "fonttbl" >> fontContent)
     where
      fontContent =
        FontTbl . fromList <$> many' (skipSpace >> decodeRTF @FontInfo <* skipSpace)
          <?> "FontTbl"

instance RTFEncoding FontInfo where
  encodeRTF :: FontInfo -> Text
  encodeRTF (FontInfo num family charset name) =
    toRTFKeyword ("f" <> showt num)
      <> encodeRTF family
      <> toOptionalKeywordWith ("fcharset" <>) (showt <$> charset)
      <> " "
      <> name
      <> charBlockEnd

  decodeRTF :: Parser FontInfo
  decodeRTF =
    FontInfo
      <$> (fromRTFKeywordWith "f" >> decimal <?> "fontNum")
      <*> decodeRTF
      <*> optional (fromRTFKeywordWith "fcharset" >> decimal <?> "fontCharset")
      <*> (char ' ' >> fontName <?> "fontName")
      <?> "FontInfo"
   where
    fontName =
      takeWhile (/= ';')
        <&> T.decodeUtf8

instance RTFEncoding ColorDef where
  encodeRTF :: ColorDef -> Text
  encodeRTF (ColorDef r g b) =
    toOptionalKeywordWith ("red" <>) r
      <> toOptionalKeywordWith ("green" <>) g
      <> toOptionalKeywordWith ("blue" <>) b
      <> charBlockEnd
  decodeRTF :: Parser ColorDef
  decodeRTF =
    ColorDef <$> c "red" <*> c "green" <*> c "blue" <* fromCharBlockend
      <?> "ColorDef"
   where
    c :: Text -> Parser (Maybe Word8)
    c s =
      optional (fromRTFKeywordWith s >> decimal)
        <?> "color"

instance RTFEncoding FontFamily where
  encodeRTF :: FontFamily -> Text
  encodeRTF = toRTFKeyword . fontFamilyText
  decodeRTF :: Parser FontFamily
  decodeRTF =
    foldr1 (<|>) (f <$> [FNil .. FBidi])
      <?> "FontFamily"
   where
    f x = fromRTFKeywordWith (fontFamilyText x) *> return x

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

fromRTFBlock :: Parser a -> Parser a
fromRTFBlock p = char '{' >> p <* (char '}' >> whitespaces)

toRTFBlock :: Text -> Text
toRTFBlock s = "{" <> s <> "}"

fromRTFKeywordWith :: Text -> Parser Text
fromRTFKeywordWith s =
  fromRTFKeyword (string (T.encodeUtf8 s) *> return s)
    <?> ("\\" <> T.unpack s)

anyRtfKeyword :: Parser Text
anyRtfKeyword = fromRTFKeyword undefined

toRTFKeyword :: Text -> Text
toRTFKeyword = ("\\" <>)

fromRTFKeyword :: Parser Text -> Parser Text
fromRTFKeyword p = char '\\' >> p

fromCharBlockend :: Parser ()
fromCharBlockend = void $ char ';'

charBlockEnd :: Text
charBlockEnd = ";"

toOptionalKeyword :: TextShow a => (Text -> Text) -> Maybe a -> Text
toOptionalKeyword f = toOptionalKeywordWith f . (showt <$>)

toOptionalKeyword_ :: TextShow a => Maybe a -> Text
toOptionalKeyword_ = toOptionalKeyword id

toOptionalKeywordWith :: TextShow a => (Text -> Text) -> Maybe a -> Text
toOptionalKeywordWith f = maybe "" (("\\" <>) . f . showt)

whitespaces :: Parser ()
whitespaces = skipWhile (\x -> isSpace x || PT.isEndOfLine x)

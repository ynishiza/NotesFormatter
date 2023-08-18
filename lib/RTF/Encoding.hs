{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTF.Encoding (
  charBlockEnd,
  RTFEncoding (..),
  ByteString,
  module X,
  debugPeek,
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString) 
import Data.ByteString qualified as B
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace
import RTF.ExtensionTypes as X
import RTF.Types as X
import Utils as X
import Prelude hiding (takeWhile)

class Generic c => RTFEncoding c where
  encodeRTF :: c -> Text
  decodeRTF :: Parser c

encodeRTFInnerControl :: RTFEncoding c => Maybe c -> Text
encodeRTFInnerControl (Just item) = encodeRTF item <> charBlockEnd
encodeRTFInnerControl Nothing = charBlockEnd

decodeRTFInnerControl :: RTFEncoding c => Parser (Maybe c)
decodeRTFInnerControl = (blockEnd >> return Nothing) <|> (Just <$> decodeRTF <* blockEnd)

-- class RTFEncodingWordType c where
--   decodeType :: Parser c

controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> T.unpack b

instance RTFEncoding RTFControlSymbol where
  encodeRTF s = T.pack [charControl, getRtfControlSymbol s]
  decodeRTF =
    char charControl *> (rtfControlSymbol <$> satisfy (notInClass charControlName))
      <?> "RTFControlSymbol"

instance RTFEncoding RTFControlWord where
  encodeRTF (RTFControlWord name NoTrailing) = controlWith name
  encodeRTF (RTFControlWord name TrailingSpace) = controlWith $ name <> " "
  encodeRTF (RTFControlWord name (RTFControlParam n)) = controlWith $ name <> showt n
  decodeRTF = decodeControlWord name
   where
    name =
      T.pack <$> many1' (satisfy (inClass charControlName))
        <?> "RTFControlWord"

instance RTFEncoding a => RTFEncoding (RTFGroup a) where
  encodeRTF (RTFGroup as) = T.intercalate "" $ encodeRTF <$> as
  decodeRTF = RTFGroup <$> decodeRTFGroup (many (decodeRTF @a))

instance RTFEncoding RTFHeader where
  encodeRTF (RTFHeader{..}) =
    encodeControlWithLabel "rtf1" False
      <> encodeRTF rtfCharset
      <> T.intercalate "" (encodeRTF <$> rtfCocoaControls)
      <> encodeRTF rtfFontTbl
      <> encodeRTF (ColorTbl (fst <$> rtfColors))
      <> encodeRTF (ExpandedColorTbl (snd <$> rtfColors))
  decodeRTF =
    decodeControlWord (text "rtf1")
      *> ( RTFHeader
            <$> (decodeRTF @Charset)
            <*> manyControls (decodeRTF @CocoaControl)
            <*> (decodeRTF @FontTbl)
            <*> colors
         )
   where
    colors = do
      ColorTbl baseColors <- decodeRTF
      ExpandedColorTbl expandedColors <- decodeRTF
      unless (length baseColors == length expandedColors) $ fail $ "Non matching color and expanded color table" <> show (length baseColors) <> show (length expandedColors)
      return $ zip baseColors expandedColors

instance RTFEncoding Charset where
  encodeRTF (Ansi n) =
    encodeControlWithLabel "ansi" False
      <> encodeControlWithValue "ansicpg" n
  decodeRTF =
    decodeControlWord (text "ansi") *> (decodeControlWordWithValue (text "ansicpg") $ \_ v -> Ansi v)
      <?> "Ansi"

instance RTFEncoding CocoaControl where
  encodeRTF (CocoaControl v) = encodeControlWithLabel ("cocoa" <> v) False
  decodeRTF = decodeControlWord (text "cocoa") *> (CocoaControl <$> alphaNum)

instance RTFEncoding FontTbl where
  encodeRTF (FontTbl infos) =
    encodeRTFGroup $
      encodeControlWithLabel "fonttbl" False
        <> T.intercalate "" (encodeRTFInnerControl <$> infos)
  decodeRTF =
    decodeRTFGroup content
      <?> "FontTbl"
   where
    content =
      decodeControlWord (text "fonttbl")
        *> (FontTbl <$> manyControls (decodeRTFInnerControl @FontInfo))

instance RTFEncoding ColorTbl where
  encodeRTF (ColorTbl colors) =
    encodeRTFGroup $
      encodeControlWithLabel "colortbl" False
        <> T.intercalate "" (appendBlockEnd . encodeRTF <$> colors)
  decodeRTF =
    decodeRTFGroup content
      <?> "ColorTbl"
   where
    content =
      decodeControlWord (text "colortbl")
        *> (ColorTbl <$> manyControls (decodeRTF @RTFColor <* blockEnd))

instance RTFEncoding ExpandedColorTbl where
  encodeRTF (ExpandedColorTbl colors) =
    encodeRTFGroup $
      encodeControlWithLabel "expandedcolortbl" False
        <> T.intercalate "" (encodeRTFInnerControl <$> colors)
  decodeRTF =
    decodeRTFGroup content
      <?> "ExpandedColorTbl"
   where
    content =
      decodeControlWord (text "expandedcolortbl")
        *> (ExpandedColorTbl <$> manyControls (decodeRTFInnerControl @ColorSpace))

instance RTFEncoding RTFText where
  encodeRTF (RTFText t) = t
  decodeRTF =
    RTFText
      . T.decodeUtf8
      <$> isNonEmpty (takeWhile (/= charControl))
      <??> "RTFText"
    where isNonEmpty :: Parser ByteString -> Parser ByteString
          isNonEmpty p = do
            d <- p
            guard $ B.length d > 0
            return d

instance RTFEncoding FontInfo where
  encodeRTF (FontInfo fontId family charset name) =
    encodeControlWithValue "f" fontId
      <> encodeRTF family
      <> maybe "" (encodeControlWithValue "fcharset") charset
      <> " "
      <> name
  decodeRTF =
    FontInfo
      <$> fontId
      <*> decodeRTF @FontFamily
      <*> optional charset
      <*> (skipSpace *> fontName <?> "fontName")
      <?> "FontInfo"
   where
    fontId =
      decodeControlWordWithValue (text "f") (\_ v -> v)
        <?> "fontNum"
    charset =
      decodeControlWordWithValue (text "fcharset") (\_ v -> v)
        <?> "fontCharset"
    fontName = toText $
      takeWhile (/= ';')

instance RTFEncoding FontFamily where
  encodeRTF f = encodeRTF $ RTFControlWord (fontFamilyText f) NoTrailing
  decodeRTF =
    choice (parseFamily <$> allColors)
      <??> "FontFamily"
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      decodeControlWord (text name)
        >> return t
        <??> name
     where
      name = fontFamilyText t

instance RTFEncoding RTFColor where
  encodeRTF (RTFColor r g b) = encode "red" r <> encode "green" g <> encode "blue" b
   where
    encode name (Just value) = encodeControlWithValue name value
    encode _ Nothing = ""
  decodeRTF =
    RTFColor <$> decode "red" <*> decode "green" <*> decode "blue"
      <?> "RTFColor"
   where
    decode name = optional $ decodeControlWordWithValue (text name) $ \_ v -> fromIntegral v

instance RTFEncoding ColorSpace where
  encodeRTF d = case d of
    CSGray v -> encodeControlWithLabel "csgray" False <> value v
    CSSRGB r g b -> encodeControlWithLabel "cssrgb" False <> value r <> value g <> value b
    CSGenericRGB r g b -> encodeControlWithLabel "csgenericrgb" False <> value r <> value g <> value b
   where
    value = encodeControlWithValue "c"
  decodeRTF =
    gray <|> cssrgb <|> genericrgb
      <?> "ColorSpace"
   where
    gray =
      decodeControlWord (text "csgray") *> (CSGray <$> value)
        <?> "CSGray"
    cssrgb =
      decodeControlWord (text "cssrgb") *> (CSSRGB <$> value <*> value <*> value)
        <?> "CSSRGB"
    genericrgb =
      decodeControlWord (text "csgenericrgb") *> (CSGenericRGB <$> value <*> value <*> value)
        <?> "CSGenericRGB"
    value = decodeControlWordWithValue (text "c") $ \_ v -> v

encodeRTFGroup :: Text -> Text
encodeRTFGroup t = "{" <> t <> "}"

decodeRTFGroup :: Parser a -> Parser a
decodeRTFGroup p =
  trimNewLines (char '{' *> trimNewLines p <* char '}')
    <?> "RTFGroup"

encodeControlWithLabel :: Text -> Bool -> Text
encodeControlWithLabel name False = encodeRTF $ RTFControlWord name NoTrailing
encodeControlWithLabel name True = encodeRTF $ RTFControlWord name TrailingSpace

encodeControlWithValue :: Integral v => Text -> v -> Text
encodeControlWithValue name v = encodeRTF $ RTFControlWord name $ RTFControlParam $ fromIntegral v

decodeControlWordWithValue :: Parser Text -> (Text -> Int -> a) -> Parser a
decodeControlWordWithValue p f = do
  RTFControlWord name (RTFControlParam v) <- decodeControlWord p
  return $ f name v

decodeControlWord :: Parser Text -> Parser RTFControlWord
decodeControlWord name = trimNewLines $ decodeControlWordBase name

decodeControlWordBase :: Parser Text -> Parser RTFControlWord
decodeControlWordBase name =
  char charControl
    *> ( RTFControlWord
          <$> (name <?> "name")
          <*> ( trailingSpace
                  <|> (RTFControlParam <$> decimal <?> "RTFControlParam")
                  <|> return NoTrailing
              )
       )
 where
  trailingSpace =
    void (satisfy isSpace) >> return TrailingSpace
      <?> "TrailingSpace"

appendBlockEnd :: Text -> Text
appendBlockEnd t = t <> charBlockEnd

toText :: Parser ByteString -> Parser Text
toText = (T.decodeUtf8 <$>)

text :: Text -> Parser Text
text value = string (T.encodeUtf8 value) *> return value

-- New lines are ignored in RTF
-- A new line plain text uses a RTF symbol instead
--
-- e.g.
--        \n        ignored
--        \\n       symbol \n
--
skipNewLines :: Parser ()
skipNewLines = void $ many (satisfy (inClass charNewline))

trimNewLines :: Parser a -> Parser a
trimNewLines p = skipNewLines *> p <* skipNewLines

manyControls :: Parser a -> Parser [a]
manyControls p = many (trimNewLines p)

-- ============================== OLD ==============================

instance RTFEncoding RTFDoc where
  encodeRTF RTFDoc{..} =
    "{"
      <> encodeRTF rtfDocHeader
      <> "\n"
      <> T.intercalate "" (encodeRTF <$> rtfDocContent)
      <> "}"

instance RTFEncoding RTFContent where
  encodeRTF (RTFContentS x) = encodeRTF x
  encodeRTF (RTFContentW x) = encodeRTF x
  encodeRTF (RTFContentG x) = encodeRTF x
  encodeRTF (RTFContentT x) = encodeRTF x
  decodeRTF =
    choice
      [ RTFContentS <$> decodeRTF @RTFControlSymbol
      , RTFContentW <$> decodeRTF @RTFControlWord
      , RTFContentG <$> decodeRTF
      , RTFContentT <$> decodeRTF @RTFText
      ]
      <?> "RTFContent"

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

debugPeek :: Parser ()
debugPeek = do
  s <- peekChar
  trace ("DEBUG" <> show s) $ return ()

blockEnd :: Parser ()
blockEnd = void $ char ';' >> skipSpace

charBlockEnd :: Text
charBlockEnd = ";"

alphaNum :: Parser Text
alphaNum =
  takeWhile (\c -> isAlpha_ascii c || isDigit c)
    & toText

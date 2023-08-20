{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTFDoc.Encoding (
  module X,
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import RTF.Encoding as X
import RTFDoc.Types as X
import Prelude hiding (takeWhile)

encodeRTFInnerControl :: RTFEncoding c => Maybe c -> Text
encodeRTFInnerControl (Just item) = encodeRTF item <> charBlockEnd
encodeRTFInnerControl Nothing = charBlockEnd

decodeRTFInnerControl :: RTFEncoding c => Parser (Maybe c)
decodeRTFInnerControl = (blockEnd >> return Nothing) <|> (Just <$> decodeRTF <* blockEnd)

instance RTFEncoding RTFDoc where
  encodeRTF RTFDoc{..} =
    "{"
      <> encodeRTF rtfDocHeader
      <> "\n"
      <> T.intercalate "" (encodeRTF <$> rtfDocContent)
      <> "}"

  decodeRTF =
    trimNewLines
      ( decodeRTFGroup $
          RTFDoc <$> trimNewLines decodeRTF <*> trimNewLines (many (trimNewLines decodeRTF))
      )
      <?> "RTFDoc"

instance RTFEncoding RTFHeader where
  encodeRTF (RTFHeader{..}) =
    encodeControlWithLabel "rtf1" False
      <> encodeRTF rtfCharset
      <> T.intercalate "" (encodeRTF <$> rtfCocoaControls)
      <> encodeRTF rtfFontTbl
      <> encodeRTF (ColorTbl (fst <$> rtfColors))
      <> encodeRTF (ExpandedColorTbl (snd <$> rtfColors))
  decodeRTF =
    decodeControlWord_ "rtf1"
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
    decodeControlWord_ "ansi" *> decodeControlWordWithValue_ "ansicpg" (\_ v -> Ansi v)
      <?> "Ansi"

instance RTFEncoding CocoaControl where
  encodeRTF (CocoaControl name (Just v)) = encodeControlWithValue ("cocoa" <> name) v
  encodeRTF (CocoaControl name Nothing) = encodeControlWithLabel ("cocoa" <> name) False
  decodeRTF = do
    (RTFControlWord name v_) <- decodeControlWord nameParse
    return $ case v_ of
      NoTrailing -> CocoaControl name Nothing
      TrailingSpace -> CocoaControl name Nothing
      RTFControlParam val -> CocoaControl name (Just val)
   where
    nameParse = parseText "cocoa" *> (T.pack <$> many (satisfy (`elem` charControlName)))

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
      decodeControlWord_ "fonttbl"
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
      decodeControlWord_ "colortbl"
        *> (ColorTbl <$> manyControls (decodeRTF @RTFColor <* blockEnd))

instance RTFEncoding ExpandedColorTbl where
  encodeRTF (ExpandedColorTbl colors) =
    encodeRTFGroup $
      toDestination (encodeControlWithLabel "expandedcolortbl" False)
        <> T.intercalate "" (encodeRTFInnerControl <$> colors)
  decodeRTF =
    decodeRTFGroup content
      <?> "ExpandedColorTbl"
   where
    content =
      withDestination (decodeControlWord_ "expandedcolortbl")
        *> (ExpandedColorTbl <$> manyControls (decodeRTFInnerControl @ColorSpace))

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
      decodeControlWordWithValue_ "f" (\_ v -> v)
        <?> "fontNum"
    charset =
      decodeControlWordWithValue_ "fcharset" (\_ v -> v)
        <?> "fontCharset"
    fontName =
      toText $
        takeWhile (/= ';')

instance RTFEncoding FontFamily where
  encodeRTF f = encodeRTF $ RTFControlWord (fontFamilyText f) NoTrailing
  decodeRTF =
    choice (parseFamily <$> allColors)
      <??> "FontFamily"
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      decodeControlWord_ name
        >> return t
        <??> name
     where
      name = fontFamilyText t

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance RTFEncoding RTFColor where
  encodeRTF (RTFColor r g b) = encode "red" r <> encode "green" g <> encode "blue" b
   where
    encode name (Just value) = encodeControlWithValue name value
    encode _ Nothing = ""
  decodeRTF =
    RTFColor <$> decode "red" <*> decode "green" <*> decode "blue"
      <?> "RTFColor"
   where
    decode name = optional $ decodeControlWordWithValue_ name $ \_ v -> fromIntegral v

instance RTFEncoding ColorSpace where
  encodeRTF word = case word of
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
      decodeControlWord_ "csgray" *> (CSGray <$> value)
        <?> "CSGray"
    cssrgb =
      decodeControlWord_ "cssrgb" *> (CSSRGB <$> value <*> value <*> value)
        <?> "CSSRGB"
    genericrgb =
      decodeControlWord_ "csgenericrgb" *> (CSGenericRGB <$> value <*> value <*> value)
        <?> "CSGenericRGB"
    value = decodeControlWordWithValue_ "c" $ \_ v -> v

encodeControlWithLabel :: Text -> Bool -> Text
encodeControlWithLabel name False = encodeRTF $ RTFControlWord name NoTrailing
encodeControlWithLabel name True = encodeRTF $ RTFControlWord name TrailingSpace

encodeControlWithValue :: Integral v => Text -> v -> Text
encodeControlWithValue name v = encodeRTF $ RTFControlWord name $ RTFControlParam $ fromIntegral v

decodeControlWordWithValue :: Parser Text -> (Text -> Int -> a) -> Parser a
decodeControlWordWithValue p f = do
  RTFControlWord name (RTFControlParam v) <- decodeControlWord p
  return $ f name v

decodeControlWordWithValue_ :: Text -> (Text -> Int -> a) -> Parser a
decodeControlWordWithValue_ name f = decodeControlWordWithValue (parseText name) f
decodeControlWord_ :: Text -> Parser RTFContent
decodeControlWord_ name = trimNewLines $ decodeControlWordBase $ parseText name

withDestination :: Parser a -> Parser a
withDestination p = string "\\*" *> p

toDestination :: Text -> Text
toDestination t = "\\*" <> t

decodeControlWordBase :: Parser Text -> Parser RTFContent
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
    void (satisfy (== ' ')) >> return TrailingSpace
      <?> "TrailingSpace"

appendBlockEnd :: Text -> Text
appendBlockEnd text = text <> charBlockEnd

toText :: Parser ByteString -> Parser Text
toText = (T.decodeUtf8 <$>)

parseText :: Text -> Parser Text
parseText value = string (T.encodeUtf8 value) *> return value

blockEnd :: Parser ()
blockEnd = void $ char ';' >> skipSpace

manyControls :: Parser a -> Parser [a]
manyControls p = many (trimNewLines p)

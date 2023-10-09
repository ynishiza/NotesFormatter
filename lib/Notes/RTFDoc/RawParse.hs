{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module Notes.RTFDoc.RawParse (
  Parseable (..),
  module X,
) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Functor
import Data.Text qualified as T
import Notes.RTF.Convert as X
import Notes.RTFDoc.Types as X
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Prelude hiding (takeWhile)

class Generic c => Parseable c where
  parse :: Parser c

instance Parseable RTFDoc where
  parse =
    trimNewLines
      ( parseRTFGroupWith $
          RTFDoc <$> trimNewLines (parse @RTFHeader) <*> parseRTFElements
      )
      <?> "RTFDoc"

instance Parseable RTFHeader where
  parse =
    parseControlWord_ "rtf1"
      *> ( RTFHeader
            <$> (parse @Charset)
            <*> manyControls (parse @CocoaControl)
            <*> (parse @FontTbl)
            <*> colors
         )
   where
    colors = do
      ColorTbl baseColors <- parse
      ExpandedColorTbl expandedColors <- parse
      unless (length baseColors == length expandedColors) $ fail $ "Non matching color and expanded color table" <> show (length baseColors) <> show (length expandedColors)
      return $ zip baseColors expandedColors

instance Parseable Charset where
  parse =
    parseControlWord_ "ansi" *> parseControlWordWithValue_ "ansicpg" (\_ v -> Ansi v)
      <?> "Ansi"

instance Parseable CocoaControl where
  parse = do
    (RTFControlWord NoPrefix name v_) <- parseRTFControlWord nameParse
    return $ case v_ of
      NoSuffix -> CocoaControl name Nothing
      SpaceSuffix -> CocoaControl name Nothing
      RTFControlParam val -> CocoaControl name (Just val)
   where
    nameParse = parseText "cocoa" *> (T.pack <$> many (satisfy (`elem` charExtendedControlName)))

instance Parseable FontTbl where
  parse =
    parseRTFGroupWith content
      <?> "FontTbl"
   where
    content =
      parseControlWord_ "fonttbl"
        *> (FontTbl <$> manyControls (parseGroupItem @FontInfo))

instance Parseable ColorTbl where
  parse =
    parseRTFGroupWith content
      <?> "ColorTbl"
   where
    content =
      parseControlWord_ "colortbl"
        *> (ColorTbl <$> manyControls (parse @RTFColor <* groupItemDelim))

instance Parseable ExpandedColorTbl where
  parse =
    parseRTFGroupWith content
      <?> "ExpandedColorTbl"
   where
    content =
      parseControlWord_ "expandedcolortbl"
        *> (ExpandedColorTbl <$> manyControls (parseGroupItem @ColorSpace))

instance Parseable FontInfo where
  parse =
    FontInfo
      <$> fontId
      <*> parse @FontFamily
      <*> optional charset
      <*> (skipSpace *> fontName <?> "fontName")
      <?> "FontInfo"
   where
    skipSpace = takeWhileP (Just "space") (inClass " ")
    fontId =
      parseControlWordWithValue_ "f" (\_ v -> v)
        <?> "fontNum"
    charset =
      parseControlWordWithValue_ "fcharset" (\_ v -> v)
        <?> "fontCharset"
    fontName =
      takeWhile1P (Just "Font name") (/= ';')

instance Parseable FontFamily where
  parse =
    choice (parseFamily <$> allValues)
      <??> "FontFamily"
   where
    allValues = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      ( parseControlWord_ name
          >> return t
      )
        <??> name
     where
      name = fontFamilyText t

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance Parseable RTFColor where
  parse =
    RTFColor <$> parseValue "red" <*> parseValue "green" <*> parseValue "blue"
      <?> "RTFColor"
   where
    parseValue name = optional $ parseControlWordWithValue_ name $ \_ v -> fromIntegral v

instance Parseable ColorSpace where
  parse =
    gray <|> cssrgb <|> genericrgb
      <?> "ColorSpace"
   where
    gray =
      parseControlWord_ "csgray" *> (CSGray <$> value)
        <?> "CSGray"
    cssrgb =
      parseControlWord_ "cssrgb" *> (CSSRGB <$> value <*> value <*> value)
        <?> "CSSRGB"
    genericrgb =
      parseControlWord_ "csgenericrgb" *> (CSGenericRGB <$> value <*> value <*> value)
        <?> "CSGenericRGB"
    value = parseControlWordWithValue_ "c" $ \_ v -> v

parseControlWordWithValue :: Parser Text -> (Text -> Int -> a) -> Parser a
parseControlWordWithValue p f = do
  RTFControlWord NoPrefix name (RTFControlParam v) <- parseRTFControlWord p
  return $ f name v

parseControlWordWithValue_ :: Text -> (Text -> Int -> a) -> Parser a
parseControlWordWithValue_ name f = parseControlWordWithValue (parseText name) f

parseControlWord_ :: Text -> Parser RTFElement
parseControlWord_ name = trimNewLines $ parseRTFControlWordBase $ parseText name

parseGroupItem :: Parseable c => Parser (Maybe c)
parseGroupItem = (groupItemDelim >> return Nothing) <|> (Just <$> parse <* groupItemDelim)

groupItemDelim :: Parser ()
groupItemDelim = void $ char ';' >> skipNewLines

manyControls :: Parser a -> Parser [a]
manyControls p = many (trimNewLines p)

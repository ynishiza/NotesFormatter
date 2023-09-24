{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module Notes.RTFDoc.ToRTFDoc (
  ToRTFDoc (..),
  parseDoc,
  parseDoc_,
  parseDocTest,
  rtfText_,
  ContentParseResult (..),
  errorBundlePretty,
) where

import Control.Lens
import Control.Monad
import Data.List (intersperse)
import Data.Text qualified as T
import Notes.RTF.ContentParser
import Notes.RTF.Convert
import Notes.RTFDoc.Types
import Text.Megaparsec

data ContentParseResult a
  = FileParseError (ParseErrorBundle Text Void)
  | ContentParseError (ParseErrorBundle [RTFContent] RTFParseError)
  | Success a
  deriving (Eq, Show)

parseDoc_ :: ContentParser c -> Text -> Either String c
parseDoc_ p d = case parseDoc p d of
  FileParseError e -> Left $ errorBundlePretty e
  ContentParseError e -> Left $ errorBundlePretty e
  Success doc -> Right doc

parseDoc :: ContentParser c -> Text -> ContentParseResult c
parseDoc p d = case runParser parseRTFContents "RTFContent" d of
  Left e -> FileParseError e
  Right contents -> case runParser p "RTFDoc" contents of
    Left e -> ContentParseError e
    Right doc -> Success doc

parseDocTest :: Show c => ContentParser c -> Text -> IO ()
parseDocTest p d = case parseDoc_ p d of
  Left e -> putStrLn e
  Right doc -> print doc

class ToRTFDoc c where
  toRTFDoc :: ContentParser c

instance ToRTFDoc RTFDoc where
  toRTFDoc = rtfGroup "RTFDoc" $ RTFDoc <$> toRTFDoc @RTFHeader <*> takeRest

instance ToRTFDoc RTFHeader where
  toRTFDoc =
    rtfControlWordValue_ "rtf" id
      *> ( RTFHeader
            <$> (toRTFDoc @Charset)
            <*> many (toRTFDoc @CocoaControl)
            <*> (toRTFDoc @FontTbl)
            <*> colors
         )
   where
    colors = do
      ColorTbl baseColors <- toRTFDoc
      ExpandedColorTbl expandedColors <- toRTFDoc
      unless (length baseColors == length expandedColors) $ fail $ "Non matching color and expanded color table" <> show (length baseColors) <> show (length expandedColors)
      return $ zip baseColors expandedColors

instance ToRTFDoc Charset where
  toRTFDoc =
    rtfControlWordLabel_ "ansi" *> rtfControlWordValue_ "ansicpg" Ansi

instance ToRTFDoc CocoaControl where
  toRTFDoc = rtfControlWord $ \_ name end ->
    case T.stripPrefix "cocoa" name of
      Just name' ->
        Right $ case end of
          RTFControlParam v -> CocoaControl name' (Just v)
          _ -> CocoaControl name' Nothing
      Nothing -> Left $ errorLabelText "cocoa"

instance ToRTFDoc ExpandedColorTbl where
  toRTFDoc =
    rtfGroupWithDelims "expandedcolortbl" content
   where
    content =
      rtfControlWordLabel_ "expandedcolortbl"
        *> (ExpandedColorTbl <$> many (try $ optional toRTFDoc <* groupItemDelimiter))

instance ToRTFDoc ColorTbl where
  toRTFDoc = ColorTbl <$> rtfGroupWithDelims "ColorTbl" content
   where
    content = rtfControlWordLabel_ "colortbl" *> many (toRTFDoc <* groupItemDelimiter)

instance ToRTFDoc RTFColor where
  toRTFDoc = RTFColor <$> red <*> green <*> blue
   where
    red = optional $ rtfControlWordValue_ "red" (fromIntegral @Int @Word8)
    green = optional $ rtfControlWordValue_ "green" (fromIntegral @Int @Word8)
    blue = optional $ rtfControlWordValue_ "blue" (fromIntegral @Int @Word8)

instance ToRTFDoc ColorSpace where
  toRTFDoc = try $ gray <|> cssrgb <|> genericrgb
   where
    gray =
      rtfControlWordLabel_ "csgray" *> (CSGray <$> value)
    cssrgb =
      rtfControlWordLabel_ "cssrgb" *> (CSSRGB <$> value <*> value <*> value)
    genericrgb =
      rtfControlWordLabel_ "csgenericrgb" *> (CSGenericRGB <$> value <*> value <*> value)
    value = rtfControlWordValue_ "c" id

instance ToRTFDoc FontTbl where
  toRTFDoc =
    rtfGroupWithDelims "FontTbl" content
   where
    content =
      rtfControlWordLabel_ "fonttbl"
        *> (FontTbl <$> many (try $ optional toRTFDoc <* groupItemDelimiter))

instance ToRTFDoc FontFamily where
  toRTFDoc = choice (parseFamily <$> allColors)
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      try $
        rtfControlWordLabel_ name
          >> return t
     where
      name = fontFamilyText t

instance ToRTFDoc FontInfo where
  toRTFDoc = FontInfo <$> fontId <*> toRTFDoc @FontFamily <*> optional charset <*> fontName
   where
    fontId = rtfControlWordValue_ "f" id
    charset = rtfControlWordValue_ "fcharset" id
    fontName = rtfText $ \t -> if T.length t > 0 then Right (T.strip t) else Left (errorLabelText "font name")

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

rtfGroupWithDelims :: Text -> ContentParser c -> ContentParser c
rtfGroupWithDelims msg p =
  updateParserState (over _stateInput prepareGroup) >> rtfGroup msg p
 where
  prepareGroup (RTFGroup c : rest) = RTFGroup (separateDelimitedGroupItems ";" c) : rest
  prepareGroup x = x

charItemDelim :: Text
charItemDelim = ";"

groupItemDelimiter :: ContentParser ()
groupItemDelimiter = rtfText_ charItemDelim *> pure ()

{- |
  Group items are often delimited with ";"
  If there are multiple empty items, it maybe be parsed as a single text.
  To count the items correctly, we need to split them

  e.g. if group is

    {\fonttbl\f0\fnil abc;;;}           3 font infos, 2 empty

  We need to split the text

    "abc;;;"   --split-->   ["abc;", ";", ";"]
-}
separateDelimitedGroupItems :: Text -> [RTFContent] -> [RTFContent]
separateDelimitedGroupItems delimiter (RTFText text : rest) =
  let
    isEmptyTerm t = t == "\n" || t == ""
    texts' =
      T.splitOn delimiter text
        & intersperse delimiter
        & filter (not . isEmptyTerm)
        <&> RTFText
   in
    texts' <> separateDelimitedGroupItems delimiter rest
separateDelimitedGroupItems delimiter (x : rest) = x : separateDelimitedGroupItems delimiter rest
separateDelimitedGroupItems _ [] = []

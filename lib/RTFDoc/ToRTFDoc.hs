{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module RTFDoc.ToRTFDoc (
  ToRTFDoc (..),
  separateDelimitedGroupItems,
  module X,
) where

import CPSParser.Combinator
import CPSParser.Types
import Data.List (intersperse)
import Data.Text qualified as T
import RTF.Types
import RTFDoc.CPSParser as X
import RTFDoc.Types

class ToRTFDoc c where
  toRTFDoc :: DocParser r c

instance ToRTFDoc RTFDoc where
  toRTFDoc = rtfGroup "RTFDoc" $ RTFDoc <$> toRTFDoc @RTFHeader <*> readRest
   where
    readRest = CParserT $ \(i, s) k _ -> k (s, (i + length s, []))

instance ToRTFDoc RTFHeader where
  toRTFDoc =
    rtfControlWordValue_ "rtf" id
      *> ( RTFHeader
            <$> (toRTFDoc @Charset)
            <*> many' (toRTFDoc @CocoaControl)
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
  toRTFDoc = rtfControlWord "cocoa" $ \_ name end ->
    case T.stripPrefix "cocoa" name of
      Just name' ->
        Just $ case end of
          RTFControlParam v -> CocoaControl name' (Just v)
          _ -> CocoaControl name' Nothing
      Nothing -> Nothing

instance ToRTFDoc ExpandedColorTbl where
  toRTFDoc =
    rtfGroupWithDelims "expandedcolortbl" content
   where
    content =
      rtfControlWordLabel_ "expandedcolortbl"
        *> (ExpandedColorTbl <$> many' (optional toRTFDoc <* groupItemDelimiter))

instance ToRTFDoc ColorTbl where
  toRTFDoc = ColorTbl <$> rtfGroupWithDelims "ColorTbl" content
   where
    content = rtfControlWordLabel "colorTbl" f *> many' (toRTFDoc <* groupItemDelimiter)
    f "colortbl" = Just ()
    f _ = Nothing

instance ToRTFDoc RTFColor where
  toRTFDoc = RTFColor <$> red <*> green <*> blue
   where
    red = optional $ rtfControlWordValue_ "red" (fromIntegral @Int @Word8)
    green = optional $ rtfControlWordValue_ "green" (fromIntegral @Int @Word8)
    blue = optional $ rtfControlWordValue_ "blue" (fromIntegral @Int @Word8)

instance ToRTFDoc ColorSpace where
  toRTFDoc = gray <|> cssrgb <|> genericrgb
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
        *> (FontTbl <$> many' (optional toRTFDoc <* groupItemDelimiter))

instance ToRTFDoc FontInfo where
  toRTFDoc = FontInfo <$> fontId <*> toRTFDoc @FontFamily <*> optional charset <*> fontName
   where
    fontId = rtfControlWordValue_ "f" id
    charset = rtfControlWordValue_ "fcharset" id
    fontName = rtfText "font name" $ \t -> if T.length t > 0 then Just (T.strip t) else Nothing

instance ToRTFDoc FontFamily where
  toRTFDoc = choice (parseFamily <$> allColors)
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      rtfControlWordLabel_ name
        >> return t
     where
      name = fontFamilyText t

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

rtfGroupWithDelims :: Text -> DocParser r c -> DocParser r c
rtfGroupWithDelims name p = rtfGroup name $ separateDelimitedGroupItems ";" *> p

charItemDelim :: Text
charItemDelim = ";"

groupItemDelimiter :: DocParser r ()
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
separateDelimitedGroupItems :: Text -> DocParser r ()
separateDelimitedGroupItems delimiter = CParserT $ \(i, content) k _ -> k ((), (i, cleans content))
 where
  cleans (RTFText text : rest) =
    let
      rm t = t == "\n" || t == ""
      ts' =
        T.splitOn delimiter text
          & intersperse delimiter
          & filter (not . rm)
          <&> RTFText
     in
      ts' <> cleans rest
  cleans (x : rest) = x : cleans rest
  cleans [] = []

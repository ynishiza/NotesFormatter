{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module RTFDoc.ToRTF (
  ToRTF (..),
  FromRTF (..),
  splitGroupDelim,
  module X,
) where

import CParser
import Data.Text qualified as T
import RTF.Types
import RTFDoc.Parse as X
import RTFDoc.Types

class FromRTF c where
  fromRTF :: c -> [RTFContent]

class ToRTF c where
  toRTF :: DocParser r c

instance FromRTF RTFColor where
  fromRTF (RTFColor r g b) = f "red" r <> f "green" g <> f "blue" b
   where
    delim = RTFText ";"
    f n (Just v) = [RTFControlWord n (RTFControlParam (fromIntegral v)), delim]
    f _ Nothing = [delim]

instance ToRTF RTFHeader where
  toRTF =
    rtfControlWordValue_ "rtf" id
      *> ( RTFHeader
            <$> (toRTF @Charset)
            <*> many' (toRTF @CocoaControl)
            <*> (toRTF @FontTbl)
            <*> colors
         )
   where
    colors = do
      ColorTbl baseColors <- toRTF
      ExpandedColorTbl expandedColors <- toRTF
      unless (length baseColors == length expandedColors) $ fail $ "Non matching color and expanded color table" <> show (length baseColors) <> show (length expandedColors)
      return $ zip baseColors expandedColors

instance ToRTF Charset where
  toRTF =
    rtfControlWordLabel_ "ansi" *> rtfControlWordValue_ "ansicpg" Ansi

instance ToRTF CocoaControl where
  toRTF = rtfControlWord "cocoa" $ \name end ->
    case T.stripPrefix "cocoa" name of
      Just name' ->
        Just $ case end of
          RTFControlParam v -> CocoaControl name' (Just v)
          _ -> CocoaControl name' Nothing
      Nothing -> Nothing

instance ToRTF ExpandedColorTbl where
  toRTF =
    rtfGroupWithDelims "expandedcolortbl" content
   where
    content =
      withDestination (rtfControlWordLabel_ "expandedcolortbl")
        *> (ExpandedColorTbl <$> many' (optional toRTF <* blockDelimiter))

instance ToRTF ColorTbl where
  toRTF = ColorTbl <$> rtfGroupWithDelims "ColorTbl" content
   where
    content = rtfControlWordLabel "colorTbl" f *> many' (toRTF <* blockDelimiter)
    f "colortbl" = Just ()
    f _ = Nothing

instance ToRTF RTFColor where
  toRTF = RTFColor <$> red <*> green <*> blue
   where
    red = optional $ rtfControlWordValue_ "red" (fromIntegral @Int @Word8)
    green = optional $ rtfControlWordValue_ "green" (fromIntegral @Int @Word8)
    blue = optional $ rtfControlWordValue_ "blue" (fromIntegral @Int @Word8)

instance ToRTF ColorSpace where
  toRTF = gray <|> cssrgb <|> genericrgb
   where
    gray =
      rtfControlWordLabel_ "csgray" *> (CSGray <$> value)
    cssrgb =
      rtfControlWordLabel_ "cssrgb" *> (CSSRGB <$> value <*> value <*> value)
    genericrgb =
      rtfControlWordLabel_ "csgenericrgb" *> (CSGenericRGB <$> value <*> value <*> value)
    value = rtfControlWordValue_ "c" id

instance ToRTF FontTbl where
  toRTF =
    rtfGroupWithDelims "FontTbl" content
   where
    content =
      rtfControlWordLabel_ "fonttbl"
        *> (FontTbl <$> many' (optional toRTF <* blockDelimiter))

instance ToRTF FontInfo where
  toRTF = FontInfo <$> fontId <*> toRTF @FontFamily <*> optional charset <*> fontName
   where
    fontId = rtfControlWordValue_ "f" id
    charset = rtfControlWordValue_ "fcharset" id
    fontName = rtfText "font name" $ \t -> if T.length t > 0 then Just (T.strip t) else Nothing

instance ToRTF FontFamily where
  toRTF = choice (parseFamily <$> allColors)
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t =
      rtfControlWordLabel_ name
        >> return t
     where
      name = fontFamilyText t

blockDelimiter :: DocParser r ()
blockDelimiter = rtfText_ ";" *> pure ()

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

withDestination :: DocParser r c -> DocParser r c
withDestination p = rtfSymbol_ '*' *> p

rtfGroupWithDelims :: Text -> DocParser r c -> DocParser r c
rtfGroupWithDelims name p = rtfGroup name $ splitGroupDelim *> p

splitGroupDelim :: DocParser r ()
splitGroupDelim = CParserT $ \(i, content) k _ -> k ((), (i, cleans content))
 where
  cleans (RTFText t : rest) =
    let
      f "" = [RTFText ";"]
      f t' = [RTFText t', RTFText ";"]
      ts = concatMap f $ init $ T.splitOn ";" t
     in
      ts <> cleans rest
  cleans (x : rest) = x : cleans rest
  cleans [] = []

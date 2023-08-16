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
import Data.Functor
import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace
import GHC.Exts (fromList)
import RTF.ExtensionTypes as X
import RTF.Types as X
import Utils as X
import Prelude hiding (takeWhile)

class RTFEncoding c where
  encodeRTF :: c -> Text
  decodeRTF :: Parser c

class RTFEncoding c => RTFItemEncoding c where
  encodeItem :: Maybe c -> Text
  encodeItem (Just item) = encodeRTF item
  encodeItem Nothing = charBlockEnd

  decodeItem :: Parser (Maybe c)
  decodeItem = (blockEnd >> return Nothing) <|> (Just <$> decodeRTF)

-- class RTFEncodingWordType c where
--   decodeType :: Parser c

charControl :: Char
charControl = '\\'
charControlName :: [Char]
charControlName = ['a' .. 'z']
charNum :: [Char]
charNum = ['0' .. '9']
charAlphaNum :: [Char]
charAlphaNum = charNum <> charControlName <> ['A' .. 'Z']
controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> T.unpack b

instance RTFEncoding RTFControlSymbol where
  encodeRTF (RTFControlSymbol s) = T.pack [charControl, s]
  decodeRTF =
    char charControl *> (RTFControlSymbol <$> satisfy (notInClass charControlName))
      <?> "RTFControlSymbol"

instance RTFEncoding RTFControlWord where
  encodeRTF (RTFControlWord name NoTrailing) = controlWith name
  encodeRTF (RTFControlWord name TrailingSpace) = controlWith $ name <> " "
  encodeRTF (RTFControlWord name (RTFControlParam n)) = controlWith $ name <> showt n
  decodeRTF = decWithName name
    -- char charControl
    --   *> ( RTFControlWord
    --         <$> name
    --         <*> ( trailingSpace
    --                 <|> RTFControlParam <$> decimal
    --                 <|> return NoTrailing
    --             )
    --      )
    --   <?> "RTFControlWord"
   where
    name =
      T.pack <$> many1' (satisfy (inClass charControlName))
    -- trailingSpace =
    --   void (satisfy isSpace) >> return TrailingSpace
    --     <?> "space"

instance RTFEncoding RTFText where
  encodeRTF (RTFText t) = t
  decodeRTF =
    RTFText . T.pack <$> many1' (satisfy (/= charControl))
      <?> "RTFText"

instance RTFEncoding FontFamily where
  encodeRTF f = encodeRTF $ RTFControlWord (fontFamilyText f) NoTrailing
  decodeRTF = choice $ parseFamily <$> allColors
   where
    allColors = [minBound .. maxBound :: FontFamily]
    parseFamily t = decWithName (toText $ string $ T.encodeUtf8 name)
      >> return t
      <??> name
        where name = fontFamilyText t

instance RTFEncoding RTFColor where
  encodeRTF (RTFColor r g b) = encode "red" r <> encode "green" g <> encode "blue" b
   where
    encode name (Just value) = encodeRTF $ RTFControlWord name $ RTFControlParam value
    encode _ Nothing = ""
  decodeRTF = RTFColor <$> decode "red" <*> decode "green" <*> decode "blue"
   where
     decode name = do 
      res <- optional $ decWithName $ toText $ string name
      case res of 
        Just (RTFControlWord _ (RTFControlParam v)) -> return $ Just v
        _ -> return Nothing

-- decodeControlWord :: (Text -> RTFControlWordEnd -> Maybe a) -> Parser a
-- decodeControlWord toType = do
--   (Just v) <- decodeOptionalControlWord toType
--   return v

-- decodeOptionalControlWord :: (Text -> RTFControlWordEnd -> Maybe a) -> Parser (Maybe a)
-- decodeOptionalControlWord toType = do
--   Just (RTFControlWord name t) <- optional $ decodeRTF @RTFControlWord
--   return $ toType name t

decWithName :: Parser Text -> Parser RTFControlWord
decWithName name =
    char charControl
      *> ( RTFControlWord
            <$> (name <?> "name")
            <*> ( trailingSpace
                    <|> RTFControlParam <$> decimal
                    <|> return NoTrailing
                )
         )
      <?> "RTFControlWord"
   where
    -- name =
    --   T.pack <$> many1' (satisfy (inClass charControlName))
    trailingSpace =
      void (satisfy isSpace) >> return TrailingSpace
        <?> "space"

appendBlockEnd :: Text -> Text
appendBlockEnd t = t <> charBlockEnd

toText :: Parser ByteString -> Parser Text
toText = (T.decodeUtf8 <$>)


-- ============================== OLD ==============================

instance RTFEncoding RTFDoc where
  encodeRTF RTFDoc{..} =
    "{"
      <> encodeRTF rtfDocHeader
      <> "\n"
      <> T.intercalate "" (encodeRTF <$> rtfDocContent)
      <> "}"

instance RTFEncoding RTFContent where
  encodeRTF RTFLiteralSlash = "\\\\\\"
  encodeRTF RTFLiteralOpenBrace = "\\{"
  encodeRTF RTFLiteralCloseBrace = "\\}"
  encodeRTF RTFNewLine = "\\\n"
  encodeRTF (RTFPlainText t) = t
  encodeRTF (RTFTag t trailing) = encodeRTFControl $ t <> s
   where
    s = case trailing of
      RTFControlParam n -> showt n
      TrailingSpace -> " "
      NoTrailing -> ""
  encodeRTF (RTFBlock t) = encodeRTFBlock t
  decodeRTF =
    choice
      [ slash
      , openBrace
      , closeBrace
      , tag
      , block
      , newline
      , plainText
      ]
      -- (skipSpace >> slash <|> openBrace <|> closeBrace <|> newline <|> tag <|> block <|> plainText)
      <?> "RTFContent"
   where
    slash =
      string "\\\\\\" >> return RTFLiteralSlash
        <?> "RTFLiteralSlash \\"
    openBrace =
      string "\\{" >> return RTFLiteralOpenBrace
        <?> "RTFLiteralOpenBrace {"
    closeBrace =
      string "\\}" >> return RTFLiteralCloseBrace
        <?> "RTFLiteralCloseBrace }"
    newline =
      char '\\' >> satisfy isSpace >> return RTFNewLine
        -- string "\\\n" >> return RTFNewLine
        <?> "RTFNewLine"
    tag =
      uncurry RTFTag <$> parseTag
        <?> "RTFTag"
    block =
      RTFBlock <$> rtfBlock' (nonEmpty "RTFBlock" $ toText $ takeWhile (/= '}'))
        <?> "RTFBlock"
    plainText =
      RTFPlainText <$> toNonEmptyText "RTFContent" (takeWhile (/= '\\'))
        <?> "RTFText"

instance RTFEncoding RTFHeader where
  encodeRTF (RTFHeader{..}) =
    encodeRTFControl "rtf1"
      <> encodeRTF rtfCharset
      <> T.intercalate "" (encodeRTF <$> rtfCocoaControls)
      <> encodeRTF rtfFontTbl
      <> encodeRTF (ColorTbl (fst <$> rtfColors))
      <> encodeRTF (ExpandedColorTbl (snd <$> rtfColors))
  decodeRTF =
    rtfHeaderControl "rtf1"
      *> ( RTFHeader
            <$> removeSurroundSpace (decodeRTF @Charset)
            <*> many' (removeSurroundSpace $ decodeRTF @CocoaControl)
            <*> removeSurroundSpace (decodeRTF @FontTbl)
            <*> colors
         )
   where
    colors = do
      ColorTbl baseColors <- removeSurroundSpace decodeRTF
      ExpandedColorTbl expandedColors <- removeSurroundSpace decodeRTF
      unless (length baseColors == length expandedColors) $ fail $ "Non matching color and expanded color table" <> show (length baseColors) <> show (length expandedColors)
      return $ zip baseColors expandedColors

instance RTFEncoding ExpandedColorTbl where
  encodeRTF (ExpandedColorTbl defs) =
    encodeRTFBlock $
      encodeRTFControl "*\\expandedcolortbl" <> T.intercalate "" (encodeItem <$> defs)

  decodeRTF = rtfBlock' (rtfHeaderControl "*\\expandedcolortbl" >> colorContent)
   where
    colorContent =
      ExpandedColorTbl <$> many' (removeSurroundSpace $ decodeItem @ColorSpace)
        <?> "ExpandedColorTbl"

instance RTFEncoding ColorTbl where
  encodeRTF (ColorTbl defs) =
    encodeRTFBlock $
      encodeRTFControl "colortbl" <> T.intercalate "" (appendBlockEnd . encodeRTF  <$> defs)
  decodeRTF = rtfBlock' (rtfHeaderControl "colortbl" >> colorContent)
   where
    colorContent =
      ColorTbl <$> many' (removeSurroundSpace $ (decodeRTF @RTFColor <* blockEnd))
        <?> "ColorTbl"

instance RTFEncoding FontTbl where
  encodeRTF (FontTbl infos) =
    encodeRTFBlock $
      encodeRTFControl "fonttbl" <> T.intercalate "" (encodeItem <$> infos)
  decodeRTF = rtfBlock' (rtfHeaderControl "fonttbl" >> fontContent)
   where
    fontContent =
      FontTbl . fromList <$> many' (removeSurroundSpace $ decodeItem @FontInfo)
        <?> "FontTbl"

instance RTFEncoding FontInfo where
  encodeRTF :: FontInfo -> Text
  encodeRTF (FontInfo num family charset name) =
    encodeRTFControl ("f" <> showt num)
      <> encodeRTF family
      <> encodeOptionalControlWith ("fcharset" <>) charset
      <> " "
      <> name
      <> charBlockEnd

  decodeRTF :: Parser FontInfo
  decodeRTF =
    FontInfo
      <$> (rtfHeaderControl "f" >> decimal <?> "fontNum")
      <*> decodeRTF @FontFamily
      <*> optional (rtfHeaderControl "fcharset" >> decimal <?> "fontCharset")
      <*> (skipSpace >> fontName <?> "fontName")
      -- <*> (fontName <?> "fontName")
      <* blockEnd
      <?> "FontInfo"
   where
    fontName =
      takeWhile (/= ';')
        <&> T.decodeUtf8

instance RTFItemEncoding FontInfo
instance RTFItemEncoding ColorSpace

-- instance RTFEncoding RTFColor where
--   encodeRTF :: RTFColor -> Text
--   encodeRTF (RTFColor r g b) =
--     encodeOptionalControlWith ("red" <>) r
--       <> encodeOptionalControlWith ("green" <>) g
--       <> encodeOptionalControlWith ("blue" <>) b
--       <> charBlockEnd
--   decodeRTF :: Parser RTFColor
--   decodeRTF =
--     RTFColor <$> c "red" <*> c "green" <*> c "blue" <* blockEnd
--       <?> "RTFColor"
--    where
--     c :: Text -> Parser (Maybe Word8)
--     c s =
--       optional (rtfHeaderControl s >> decimal)
--         <?> "color"

-- instance RTFEncoding FontFamily where
--   encodeRTF :: FontFamily -> Text
--   encodeRTF = encodeRTFControl . fontFamilyText
--   decodeRTF :: Parser FontFamily
--   decodeRTF =
--     foldr1 (<|>) (f <$> [FNil .. FBidi])
--       <?> "FontFamily"
--    where
--     f x = rtfHeaderControl (fontFamilyText x) *> return x

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance RTFEncoding Charset where
  encodeRTF (Ansi n) = encodeRTFControl "ansi" <> encodeRTFControl ("ansicpg" <> showt n)
  decodeRTF =
    rtfHeaderControl "ansi" *> (Ansi <$> (rtfHeaderControl "ansicpg" >> decimal))
      <?> "Ansi"

instance RTFEncoding CocoaControl where
  encodeRTF (CocoaControl v) = encodeRTFControl "cocoa" <> v
  decodeRTF = rtfHeaderControl "cocoa" *> (CocoaControl <$> alphaNum)

instance RTFEncoding ColorSpace where
  encodeRTF (CSGray v) = encodeRTFControl "csgray" <> encodeCSValue v <> charBlockEnd
  encodeRTF (CSSRGB r g b) = encodeRTFControl "cssrgb" <> encodeCSValue r <> encodeCSValue g <> encodeCSValue b <> charBlockEnd
  encodeRTF (CSGenericRGB r g b) = encodeRTFControl "csgenericrgb" <> encodeCSValue r <> encodeCSValue g <> encodeCSValue b <> charBlockEnd
  decodeRTF = gray <|> srgb <|> genericrgb
   where
    gray = rtfHeaderControl "csgray" >> CSGray <$> csvalue <* blockEnd
    srgb = rtfHeaderControl "cssrgb" >> CSSRGB <$> csvalue <*> csvalue <*> csvalue <* blockEnd
    genericrgb = rtfHeaderControl "csgenericrgb" >> CSGenericRGB <$> csvalue <*> csvalue <*> csvalue <* blockEnd
    csvalue = rtfHeaderControl "c" >> decimal

encodeCSValue :: CSValue -> Text
encodeCSValue v = encodeRTFControl "c" <> showt v

removeSurroundSpace :: Parser a -> Parser a
removeSurroundSpace p = skipSpace *> p <* skipSpace

-- rtfBlock :: Parser a -> Parser a
-- rtfBlock p =
--   (skipSpace *> rtfBlock' p <* skipSpace)
--     <?> "{ }"

rtfBlock' :: Parser a -> Parser a
rtfBlock' p =
  char '{'
    *> p
    <* (skipSpace >> char '}')
    <?> "{ }"

debugPeek :: Parser ()
debugPeek = do
  s <- peekChar
  trace ("DEBUG" <> show s) $ return ()

encodeRTFBlock :: Text -> Text
encodeRTFBlock s = "{" <> s <> "}"

rtfHeaderControl :: Text -> Parser Text
rtfHeaderControl s =
  skipSpace *> rtfControlBase (string (T.encodeUtf8 s) *> return s)
    <?> ("\\" <> T.unpack s)

encodeRTFControl :: Text -> Text
encodeRTFControl = ("\\" <>)

rtfControlBase :: Parser Text -> Parser Text
rtfControlBase p =
  char '\\' >> p
    <?> "RTF Control"

blockEnd :: Parser ()
blockEnd = void $ char ';' >> skipSpace

charBlockEnd :: Text
charBlockEnd = ";"

encodeOptionalControlWith :: TextShow a => (Text -> Text) -> Maybe a -> Text
encodeOptionalControlWith f = maybe "" (("\\" <>) . f . showt)

alphaNum :: Parser Text
alphaNum =
  takeWhile (\c -> isAlpha_ascii c || isDigit c)
    & toText

nonEmpty :: String -> Parser Text -> Parser Text
nonEmpty name p = do
  t <- p
  when (T.null t) $ fail $ "[" <> name <> "] Empty text"
  return t

toNonEmptyText :: String -> Parser ByteString -> Parser Text
toNonEmptyText name = nonEmpty name . toText

{-
  From RTF Spec: https://www.biblioscape.com/rtf15_spec.htm#Heading2

  >>>
     A control word is a specially formatted command that RTF uses to mark printer control codes and information that applications use to manage documents. A control word cannot be longer than 32 characters. A control word takes the following form:

        \LetterSequence<Delimiter>

    Note that a backslash begins each control word.

    The LetterSequence is made up of lowercase alphabetic characters between "a" and "z" inclusive. RTF is case sensitive, and all RTF control words must be lowercase.

    The delimiter marks the end of an RTF control word, and can be one of the following:

    * A space. In this case, the space is part of the control word.

    * A digit or a hyphen (-), which indicates that a numeric parameter follows. The subsequent digital sequence is then delimited by a space or any character other than a letter or a digit. The parameter can be a positive or a negative number. The range of the values for the number is generally -32767 through 32767. However, Word tends to restrict the range to -31680 through 31680. Word allows values in the range -2,147,483,648 to 2,147,483,648 for a small number of keywords (specifically \bin, \revdttm, and some picture properties). An RTF parser must handle an arbitrary string of digits as a legal value for a keyword. If a numeric parameter immediately follows the control word, this parameter becomes part of the control word. The control word is then delimited by a space or a nonalphabetic or nonnumeric character in the same manner as any other control word.

    * Any character other than a letter or a digit. In this case, the delimiting character terminates the control word but is not actually part of the control word.

    If a space delimits the control word, the space does not appear in the document. Any characters following the delimiter, including spaces, will appear in the document. For this reason, you should use spaces only where necessary; do not use spaces merely to break up RTF code.

    A control symbol consists of a backslash followed by a single, nonalphabetic character. For example, \~ represents a nonbreaking space. Control symbols take no delimiters.
  >>>
-}
parseTag :: Parser (Text, RTFControlWordEnd)
parseTag =
  char '\\'
    *> choice
      [ -- case: end with space. In this case space is part of the control
        (,TrailingSpace) <$> tagName <* satisfy isSpace
      , -- case: end with number
        (,) <$> tagName <*> (RTFControlParam <$> tagNum)
      , (,NoTrailing) <$> tagName
      ]
 where
  tagName = T.pack <$> many1' letter_ascii
  tagNum = decimal @Word8

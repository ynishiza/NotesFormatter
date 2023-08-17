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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace
import GHC.Exts (fromList)
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

charControl :: Char
charControl = '\\'
charControlName :: [Char]
charControlName = ['a' .. 'z']
charNum :: [Char]
charNum = ['0' .. '9']

charNewline = ['\n', '\r', '\f']

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
  decodeRTF = decodeControlWord name
   where
    name =
      T.pack <$> many1' (satisfy (inClass charControlName))
        <?> "RTFControlWord"

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
    rtfHeaderControl "ansi" *> (Ansi <$> (rtfHeaderControl "ansicpg" >> decimal))
      <?> "Ansi"

instance RTFEncoding CocoaControl where
  encodeRTF (CocoaControl v) = encodeControlWithLabel ("cocoa" <> v) False
  decodeRTF = rtfHeaderControl "cocoa" *> (CocoaControl <$> alphaNum)

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
      . T.pack
      <$> many1' (satisfy (/= charControl))
      <??> "RTFText"

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
    fontName =
      takeWhile (/= ';')
        <&> T.decodeUtf8

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

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

removeSurroundSpace :: Parser a -> Parser a
removeSurroundSpace p = skipSpace *> p <* skipSpace

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
  tagNum = decimal @Int

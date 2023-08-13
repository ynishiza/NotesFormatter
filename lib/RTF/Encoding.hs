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

class RTFEncoding c where
  encodeRTF :: c -> Text
  decodeRTF :: Parser c

class RTFEncoding c => RTFItemEncoding c where
  encodeItem :: Maybe c -> Text
  encodeItem (Just item) = encodeRTF item
  encodeItem Nothing = charBlockEnd

  decodeItem :: Parser (Maybe c)
  decodeItem = (blockEnd >> return Nothing) <|> (Just <$> decodeRTF)

instance RTFEncoding RTFDoc where
  encodeRTF RTFDoc {..} = "{" <> encodeRTF rtfDocHeader <> "\n" <> 
      T.intercalate "" (encodeRTF <$> rtfDocContent) <> "}"

instance RTFEncoding RTFContent where
  encodeRTF RTFLiteralSlash = "\\\\\\"
  encodeRTF RTFLiteralOpenBrace = "\\{"
  encodeRTF RTFLiteralCloseBrace = "\\}"
  encodeRTF RTFNewLine = "\\\n"
  encodeRTF (RTFText t) = t
  encodeRTF (RTFTag t trailing) = encodeRTFControl $ t <> s
   where
    s = case trailing of
      TagParameter n -> showt n
      TrailingSpace -> " "
      TrailingSymbol -> ""
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
        <?> "RTFLiteralSlash //"
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
      RTFText <$> toNonEmptyText "RTFContent" (takeWhile (/= '\\'))
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
    rtfControlWith "rtf1"
      *> ( RTFHeader
            <$> decodeRTF @Charset
            <*> many' (decodeRTF @CocoaControl)
            <*> decodeRTF @FontTbl
            <*> colors
         )
   where
    colors = do
      ColorTbl cs <- decodeRTF @ColorTbl
      ExpandedColorTbl ec <- decodeRTF
      guard $ length cs == length ec
      return $ zip cs ec

instance RTFEncoding ExpandedColorTbl where
  encodeRTF (ExpandedColorTbl defs) =
    encodeRTFBlock $
      encodeRTFControl "*\\expandedcolortbl" <> T.intercalate "" (encodeItem <$> defs)

  decodeRTF = rtfBlock (rtfControlWith "*\\expandedcolortbl" >> colorContent)
   where
    colorContent =
      ExpandedColorTbl <$> many' (skipSpace *> decodeItem @ColorSpace <* skipSpace)
        <?> "ExpandedColorTbl"

instance RTFEncoding ColorTbl where
  encodeRTF (ColorTbl defs) =
    encodeRTFBlock $
      encodeRTFControl "colortbl" <> T.intercalate "" (encodeRTF <$> defs)
  decodeRTF = rtfBlock (rtfControlWith "colortbl" >> colorContent)
   where
    colorContent =
      ColorTbl <$> many' (skipSpace *> decodeRTF @RTFColor <* skipSpace)
        <?> "ColorTbl"

instance RTFEncoding FontTbl where
  encodeRTF (FontTbl infos) =
    encodeRTFBlock $
      encodeRTFControl "fonttbl" <> T.intercalate "" (encodeItem <$> infos)
  decodeRTF = rtfBlock (rtfControlWith "fonttbl" >> fontContent)
   where
    fontContent =
      FontTbl . fromList <$> many' (skipSpace >> decodeItem @FontInfo <* skipSpace)
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
      <$> (rtfControlWith "f" >> decimal <?> "fontNum")
      <*> decodeRTF
      <*> optional (rtfControlWith "fcharset" >> decimal <?> "fontCharset")
      <*> (char ' ' >> fontName <?> "fontName")
      <* blockEnd
      <?> "FontInfo"
   where
    fontName =
      takeWhile (/= ';')
        <&> T.decodeUtf8

instance RTFItemEncoding FontInfo
instance RTFItemEncoding ColorSpace

instance RTFEncoding RTFColor where
  encodeRTF :: RTFColor -> Text
  encodeRTF (RTFColor r g b) =
    encodeOptionalControlWith ("red" <>) r
      <> encodeOptionalControlWith ("green" <>) g
      <> encodeOptionalControlWith ("blue" <>) b
      <> charBlockEnd
  decodeRTF :: Parser RTFColor
  decodeRTF =
    RTFColor <$> c "red" <*> c "green" <*> c "blue" <* blockEnd
      <?> "RTFColor"
   where
    c :: Text -> Parser (Maybe Word8)
    c s =
      optional (rtfControlWith s >> decimal)
        <?> "color"

instance RTFEncoding FontFamily where
  encodeRTF :: FontFamily -> Text
  encodeRTF = encodeRTFControl . fontFamilyText
  decodeRTF :: Parser FontFamily
  decodeRTF =
    foldr1 (<|>) (f <$> [FNil .. FBidi])
      <?> "FontFamily"
   where
    f x = rtfControlWith (fontFamilyText x) *> return x

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance RTFEncoding Charset where
  encodeRTF (Ansi n) = encodeRTFControl "ansi" <> encodeRTFControl ("ansicpg" <> showt n)
  decodeRTF =
    rtfControlWith "ansi" *> (Ansi <$> (rtfControlWith "ansicpg" >> decimal))
      <?> "Ansi"

instance RTFEncoding CocoaControl where
  encodeRTF (CocoaControl v) = encodeRTFControl "cocoa" <> v
  decodeRTF = rtfControlWith "cocoa" *> (CocoaControl <$> alphaNum)

instance RTFEncoding ColorSpace where
  encodeRTF (CSGray v) = encodeRTFControl "csgray" <> encodeCSValue v <> charBlockEnd
  encodeRTF (CSSRGB r g b) = encodeRTFControl "cssrgb" <> encodeCSValue r <> encodeCSValue g <> encodeCSValue b <> charBlockEnd
  encodeRTF (CSGenericRGB r g b) = encodeRTFControl "csgenericrgb" <> encodeCSValue r <> encodeCSValue g <> encodeCSValue b <> charBlockEnd
  decodeRTF = gray <|> srgb <|> genericrgb
   where
    gray = rtfControlWith "csgray" >> CSGray <$> csvalue <* blockEnd
    srgb = rtfControlWith "cssrgb" >> CSSRGB <$> csvalue <*> csvalue <*> csvalue <* blockEnd
    genericrgb = rtfControlWith "csgenericrgb" >> CSGenericRGB <$> csvalue <*> csvalue <*> csvalue <* blockEnd
    csvalue = rtfControlWith "c" >> decimal

encodeCSValue :: CSValue -> Text
encodeCSValue v = encodeRTFControl "c" <> showt v

rtfBlock :: Parser a -> Parser a
rtfBlock p =
  (skipSpace *> rtfBlock' p <* skipSpace)
    <?> "{ }"

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

rtfControlWith :: Text -> Parser Text
rtfControlWith s =
  rtfControl (string (T.encodeUtf8 s) *> return s)
    <?> ("\\" <> T.unpack s)

encodeRTFControl :: Text -> Text
encodeRTFControl = ("\\" <>)

rtfControl :: Parser Text -> Parser Text
rtfControl p =
  skipSpace *> rtfControl' p
    <?> "RTFControl"
rtfControl' :: Parser Text -> Parser Text
rtfControl' p =
  char '\\' >> p
    <?> "RTFControl"

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

toText :: Parser ByteString -> Parser Text
toText = (T.decodeUtf8 <$>)

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
parseTag :: Parser (Text, RTFTagEnd)
parseTag =
  char '\\'
    *> choice
      [ -- case: end with space. In this case space is part of the control
        (,TrailingSpace) <$> tagName <* satisfy isSpace
      , -- case: end with number
        (,) <$> tagName <*> (TagParameter <$> tagNum)
      , (,TrailingSymbol) <$> tagName
      ]
 where
  tagName = T.pack <$> many1' letter_ascii
  tagNum = decimal @Word8

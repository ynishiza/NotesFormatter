{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module Notes.RTF.Convert (
  charBlockEnd,
  ByteString,
  -- parse
  parseRTFControlWord,
  parseRTFControlWordBase,
  parseRTFGroupWith,
  parseRTFContent,
  parseRTFContents,
  -- render
  renderRTFGroup,
  renderRTFContent,
  -- utils
  debugPeek,
  parseText,
  (<??>),
  skipNewLines,
  trimNewLines,
  notNewLine,
  module X,
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
import Notes.RTF.Types as X
import Notes.Utils as X
import Prelude hiding (takeWhile)

controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> T.unpack b

renderRTFContent :: RTFContent -> Text
renderRTFContent (RTFControlSymbol symbol) = T.pack [charControl, symbol]
renderRTFContent (RTFControlWord prefix name suffix) = renderPrefix prefix <> controlWith name <> renderSuffix suffix
renderRTFContent (RTFGroup content) = renderRTFGroup $ T.intercalate "" $ renderRTFContent <$> content
renderRTFContent (RTFText text) = text

renderPrefix :: RTFControlPrefix -> Text
renderPrefix NoPrefix = ""
renderPrefix StarPrefix = controlWith "*"

renderSuffix :: RTFControlSuffix -> Text
renderSuffix NoSuffix = ""
renderSuffix SpaceSuffix = " "
renderSuffix (RTFControlParam n) = showt n

renderRTFGroup :: Text -> Text
renderRTFGroup t = "{" <> t <> "}"

parseRTFContents :: Parser [RTFContent]
parseRTFContents = filter notNewLine <$> many (trimNewLines parseRTFContent)

notNewLine :: RTFContent -> Bool
notNewLine (RTFText "\n") = False
notNewLine _ = True

parseRTFContent :: Parser RTFContent
parseRTFContent =
  parseRTFControlWord wordName
    <|> group
    <|> symbol
    <|> text
    <?> "RTFContent"
 where
  symbol =
    char charControl *> (rtfControlSymbol <$> satisfy (notInClass charControlName))
      <?> "RTFControlSymbol"
  wordName =
    T.pack <$> many1' (satisfy (inClass charControlName))
      <?> "RTFControlWord"
  group =
    RTFGroup <$> parseRTFGroupWith (many parseRTFContent)
      <?> "RTFGroup"
  text =
    RTFText
      . T.decodeUtf8
      <$> isNonEmpty (takeWhile (not . (`elem` charReserved)))
      <??> "RTFText"
  isNonEmpty :: Parser ByteString -> Parser ByteString
  isNonEmpty p = do
    d <- p
    guard $ B.length d > 0
    return d

parseRTFGroupWith :: Parser a -> Parser a
parseRTFGroupWith p =
  trimNewLines (char '{' *> trimNewLines p <* char '}')

parseRTFControlWord :: Parser Text -> Parser RTFContent
parseRTFControlWord name = trimNewLines $ parseRTFControlWordBase name

parseRTFControlWordBase :: Parser Text -> Parser RTFContent
parseRTFControlWordBase name =
  RTFControlWord
    <$> prefix
    <*> (char charControl *> name <?> "name")
    <*> ( trailingSpace
            <|> (RTFControlParam <$> decimal <?> "RTFControlParam")
            <|> return NoSuffix
        )
 where
  prefix =
    (char charControl *> char '*' *> return StarPrefix) <|> return NoPrefix
      <?> "RTFControlPrefix"
  trailingSpace =
    void (satisfy (== ' ')) >> return SpaceSuffix
      <?> "SpaceSuffix"

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

debugPeek :: Parser ()
debugPeek = do
  s <- peekChar
  trace ("DEBUG" <> show s) $ return ()

charBlockEnd :: Text
charBlockEnd = ";"

parseText :: Text -> Parser Text
parseText value = string (T.encodeUtf8 value) *> return value

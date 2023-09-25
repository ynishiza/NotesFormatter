{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{-|
  See README for RTF specs
-}
module Notes.RTF.Convert (
  charBlockEnd,
  ByteString,
  parseRTFControlWord,
  parseRTFControlWordBase,
  parseRTFGroupWith,
  parseRTFContent,
  parseRTFContents,
  renderRTFGroup,
  renderRTFContent,
  parseText,
  (<??>),
  skipNewLines,
  trimNewLines,
  notNewLine,
  inClass,
  module X,
  Parser,
) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Functor
import Data.Text qualified as T
import Data.Void (Void)
import Notes.RTF.Types as X
import Notes.Utils as X
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (takeWhile)
import Data.Set qualified as S

type Parser = Parsec Void Text

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
parseRTFContents =
  -- Note: newline is ignored in RTF
  filter notNewLine <$> many (trimNewLines parseRTFContent)

notNewLine :: RTFContent -> Bool
notNewLine (RTFText "\n") = False
notNewLine _ = True

parseRTFContent :: Parser RTFContent
parseRTFContent =
  group
    <|> parseRTFControlWord wordName
    <|> symbol
    <|> text
    <?> "RTFContent"
 where
  symbol =
    (do
      -- Note: wrap in 
      c <- try $ char charControl >> satisfy (notInClass charExtendedControlName)
      o <- getOffset
      case rtfControlSymbol c of
        Right v -> return v
        Left e -> do 
          let err = FancyError (o - 1) $ S.singleton $ ErrorFail e
          parseError err
    )
      <?> "RTFControlSymbol"
  wordName =
    takeWhile1P (Just "name character") (inClass charExtendedControlName)
      <?> "RTFControlWord"
  group =
    RTFGroup <$> parseRTFGroupWith (many parseRTFContent)
      <?> "RTFGroup"
  text =
    RTFText
      <$> isNonEmpty (takeWhile1P (Just "text content") (not . (`elem` charReserved)))
      <??> "RTFText"
  isNonEmpty :: Parser Text -> Parser Text
  isNonEmpty p = do
    d <- p
    guard $ T.length d > 0
    return d

parseRTFGroupWith :: Parser a -> Parser a
parseRTFGroupWith p =
  try $ trimNewLines $ between (char '{') (char '}') $ trimNewLines p

parseRTFControlWord :: Parser Text -> Parser RTFContent
parseRTFControlWord name = trimNewLines $ parseRTFControlWordBase name

parseRTFControlWordBase :: Parser Text -> Parser RTFContent
parseRTFControlWordBase name =
  try $
    RTFControlWord
      <$> prefix
      <*> (char charControl *> name <?> "name")
      <*> ( trailingSpace
              <|> (RTFControlParam <$> decimal <?> "RTFControlParam")
              <|> return NoSuffix
          )
 where
  prefix =
    (string' (controlWith "*") *> return StarPrefix) <|> return NoPrefix
      <?> "RTFControlPrefix"
  trailingSpace =
    char ' ' >> return SpaceSuffix
      <?> "SpaceSuffix"

-- New lines are ignored in RTF
-- A new line plain text uses a RTF symbol instead
--
-- e.g.
--        \n        ignored
--        \\n       symbol \n
--
-- See README for the RTF specs.
skipNewLines :: Parser ()
skipNewLines = void $ takeWhileP (Just "newline") (inClass charNewline)

trimNewLines :: Parser a -> Parser a
trimNewLines p = skipNewLines *> p <* skipNewLines

charBlockEnd :: Text
charBlockEnd = ";"

parseText :: Text -> Parser Text
parseText value = string value

inClass :: String -> Char -> Bool
inClass = flip elem

decimal :: Num a => Parser a
decimal = L.signed (return ()) L.decimal

notInClass :: String -> Char -> Bool
notInClass x = not . inClass x

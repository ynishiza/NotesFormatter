{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTF.Convert (
  charBlockEnd,
  -- RTFEncoding (..),
  ByteString,
  parseRTFControlWord,
  parseRTFGroupWith,
  renderRTFGroup,
  skipNewLines,
  trimNewLines,
  debugPeek,
  (<??>),
  module X,
  renderRTFContent,
  parseRTFContent,
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
import RTF.Types as X
import Utils as X
import Prelude hiding (takeWhile)

-- class Generic c => RTFEncoding c where
--   renderRTF :: c -> Text
--   decodeRTF :: Parser c

controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> T.unpack b

-- instance RTFEncoding RTFContent where
--   renderRTF (RTFControlSymbol symbol) = T.pack [charControl, symbol]
--   renderRTF (RTFControlWord name NoTrailing) = controlWith name
--   renderRTF (RTFControlWord name TrailingSpace) = controlWith $ name <> " "
--   renderRTF (RTFControlWord name (RTFControlParam n)) = controlWith $ name <> showt n
--   renderRTF (RTFGroup content) = renderRTFGroup $ T.intercalate "" $ renderRTF <$> content
--   renderRTF (RTFText text) = text

--   decodeRTF =
--     choice
--       [ symbol
--       , parseRTFControlWord wordName
--       , group
--       , text
--       ]
--       <?> "RTFContent"
--    where
--     symbol =
--       char charControl *> (rtfControlSymbol <$> satisfy (notInClass charControlName))
--         <?> "RTFControlSymbol"
--     wordName =
--       T.pack <$> many1' (satisfy (inClass charControlName))
--         <?> "RTFControlWord"
--     group =
--       RTFGroup <$> parseRTFGroupWith (many decodeRTF)
--         <?> "RTFGroup"
--     text =
--       RTFText
--         . T.decodeUtf8
--         <$> isNonEmpty (takeWhile (not . (`elem` charReserved)))
--         <??> "RTFText"
--     isNonEmpty :: Parser ByteString -> Parser ByteString
--     isNonEmpty p = do
--       d <- p
--       guard $ B.length d > 0
--       return d

renderRTFContent :: RTFContent -> Text
renderRTFContent (RTFControlSymbol symbol) = T.pack [charControl, symbol]
renderRTFContent (RTFControlWord name NoTrailing) = controlWith name
renderRTFContent (RTFControlWord name TrailingSpace) = controlWith $ name <> " "
renderRTFContent (RTFControlWord name (RTFControlParam n)) = controlWith $ name <> showt n
renderRTFContent (RTFGroup content) = renderRTFGroup $ T.intercalate "" $ renderRTFContent <$> content
renderRTFContent (RTFText text) = text

renderRTFGroup :: Text -> Text
renderRTFGroup t = "{" <> t <> "}"

parseRTFContent :: Parser RTFContent
parseRTFContent =
  choice
    [ symbol
    , parseRTFControlWord wordName
    , group
    , text
    ]
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
parseRTFControlWord name = trimNewLines $ parseControlWordBase name

parseControlWordBase :: Parser Text -> Parser RTFContent
parseControlWordBase name =
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

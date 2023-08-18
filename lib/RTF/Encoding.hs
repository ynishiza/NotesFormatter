{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTF.Encoding (
  charBlockEnd,
  RTFEncoding (..),
  ByteString,
  decodeControlWord,
  decodeRTFGroup,
  encodeRTFGroup,
  skipNewLines,
  trimNewLines,
  debugPeek,
  (<??>),
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
import RTF.Types as X
import Utils as X
import Prelude hiding (takeWhile)

class Generic c => RTFEncoding c where
  encodeRTF :: c -> Text
  decodeRTF :: Parser c

-- class RTFEncodingWordType c where
--   decodeType :: Parser c

controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

(<??>) :: Parser a -> Text -> Parser a
a <??> b = a <?> T.unpack b

instance RTFEncoding RTFControlSymbol where
  encodeRTF s = T.pack [charControl, getRtfControlSymbol s]
  decodeRTF =
    char charControl *> (rtfControlSymbol <$> satisfy (notInClass charControlName))
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

instance RTFEncoding a => RTFEncoding (RTFGroup a) where
  encodeRTF (RTFGroup content) = encodeRTFGroup $ T.intercalate "" $ encodeRTF <$> content
  decodeRTF = RTFGroup <$> decodeRTFGroup (many (decodeRTF @a))

instance RTFEncoding RTFContent where
  encodeRTF (RTFContentS x) = encodeRTF x
  encodeRTF (RTFContentW x) = encodeRTF x
  encodeRTF (RTFContentG x) = encodeRTF x
  encodeRTF (RTFContentT x) = encodeRTF x
  decodeRTF =
    choice
      [ RTFContentS <$> decodeRTF @RTFControlSymbol
      , RTFContentW <$> decodeRTF @RTFControlWord
      , RTFContentG <$> decodeRTF
      , RTFContentT <$> decodeRTF @RTFText
      ]
      <?> "RTFContent"

instance RTFEncoding RTFText where
  encodeRTF (RTFText t) = t
  decodeRTF =
    RTFText
      . T.decodeUtf8
      <$> isNonEmpty (takeWhile (not . (`elem` charReserved)))
      <??> "RTFText"
   where
    isNonEmpty :: Parser ByteString -> Parser ByteString
    isNonEmpty p = do
      d <- p
      guard $ B.length d > 0
      return d

encodeRTFGroup :: Text -> Text
encodeRTFGroup t = "{" <> t <> "}"

decodeRTFGroup :: Parser a -> Parser a
decodeRTFGroup p =
  trimNewLines (char '{' *> trimNewLines p <* char '}')
    <?> "RTFGroup"

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

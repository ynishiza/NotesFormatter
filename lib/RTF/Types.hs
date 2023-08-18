module RTF.Types (
  RTFGroup (..),
  RTFControlWord (..),
  RTFControlSymbol,
  rtfControlSymbol,
  getRtfControlSymbol,
  RTFText (..),
  _NoTrailing,
  _RTFControlParam,
  _TrailingSpace,
  --
  RTFContent (..),
  Word8,
  Text,
  Parser,
  RTFControlWordEnd (..),
  -- Lens
  _RTFControlWord,
  _RTFControlSymbol,
  _RTFContentS,
  _RTFContentW,
  _RTFContentG,
  _RTFContentT,
  --
  charControl,
  charControlName,
  charSymbol,
  charReserved,
  charNewline,
) where

import Data.Attoparsec.ByteString.Char8
import Data.Char (toUpper)
import Data.Word
import Utils

charControl :: Char
charControl = '\\'

charReserved :: String
charReserved = "\\{}"

charControlName :: String
charControlName = ['a' .. 'z']

charNewline :: String
charNewline = ['\n', '\r', '\f']

charNonSymbol :: String
charNonSymbol = ['0' .. '9'] <> charControlName <> (toUpper <$> charControlName)

-- 8 bits
charSymbol :: String
charSymbol = filter (not . (`elem` charNonSymbol)) $ toEnum <$> [0 .. 127]

data RTFControlWord = RTFControlWord Text RTFControlWordEnd
  deriving stock (Eq, Show, Generic)

data RTFControlWordEnd = RTFControlParam Int | TrailingSpace | NoTrailing
  deriving stock (Eq, Show, Generic)

newtype RTFControlSymbol = RTFControlSymbol Char
  deriving stock (Eq, Show, Generic)

getRtfControlSymbol :: RTFControlSymbol -> Char
getRtfControlSymbol (RTFControlSymbol s) = s
rtfControlSymbol :: Char -> RTFControlSymbol
rtfControlSymbol c =
  if c `elem` charSymbol
    then RTFControlSymbol c
    else error $ "Invalid symbol " <> ['\'', c, '\'']

newtype RTFGroup a = RTFGroup [a]
  deriving stock (Eq, Show, Generic)

newtype RTFText = RTFText Text
  deriving stock (Eq, Show, Generic)

data RTFContent
  = RTFContentS RTFControlSymbol
  | RTFContentW RTFControlWord
  | RTFContentG (RTFGroup RTFContent)
  | RTFContentT RTFText
  deriving stock (Eq, Show, Generic)

$(makePrisms ''RTFContent)
$(makePrisms ''RTFControlWord)
$(makePrisms ''RTFControlSymbol)
$(makePrisms ''RTFControlWordEnd)

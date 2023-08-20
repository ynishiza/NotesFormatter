module RTF.Types (
  rtfControlSymbol,
  getRtfControlSymbol,
  _SpaceSuffix,
  _RTFControlParam,
  _NoSuffix,
  --
  RTFContent (..),
  Word8,
  Text,
  Parser,
  RTFControlPrefix (..),
  RTFControlSuffix (..),
  -- Lens
  _RTFControlWord,
  _RTFControlSymbol,
  _RTFGroup,
  _RTFText,
  --
  charControl,
  charControlName,
  charSymbol,
  charReserved,
  charNewline,
  charOptionalDestination,
) where

import Data.Attoparsec.ByteString.Char8
import Data.Char (toUpper)
import Data.Word
import Utils

charControl :: Char
charControl = '\\'

charReserved :: String
charReserved = "\\{}"

charOptionalDestination :: Char
charOptionalDestination = '*'

charControlName :: String
charControlName = ['a' .. 'z']

charNewline :: String
charNewline = ['\n', '\r', '\f']

charNonSymbol :: String
charNonSymbol = ['0' .. '9'] <> [charOptionalDestination] <> charControlName <> (toUpper <$> charControlName)

-- 8 bits
charSymbol :: String
charSymbol = filter (not . (`elem` charNonSymbol)) $ toEnum <$> [0 .. 127]

data RTFControlPrefix = StarPrefix | NoPrefix
  deriving stock (Eq, Show, Generic)

data RTFControlSuffix = RTFControlParam Int | SpaceSuffix | NoSuffix
  deriving stock (Eq, Show, Generic)

getRtfControlSymbol :: RTFContent -> Maybe Char
getRtfControlSymbol (RTFControlSymbol s) = Just s
getRtfControlSymbol _ = Nothing

rtfControlSymbol :: Char -> RTFContent
rtfControlSymbol c =
  if c `elem` charSymbol
    then RTFControlSymbol c
    else error $ "Invalid symbol " <> ['\'', c, '\'']

data RTFContent
  = RTFControlSymbol Char
  | RTFControlWord RTFControlPrefix Text RTFControlSuffix
  | RTFGroup [RTFContent]
  | RTFText Text
  deriving stock (Eq, Show, Generic)

$(makePrisms ''RTFContent)
$(makePrisms ''RTFControlSuffix)

module RTF.Types (
  rtfControlSymbol,
  getRtfControlSymbol,
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
  _RTFGroup,
  _RTFText,
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

data RTFControlWordEnd = RTFControlParam Int | TrailingSpace | NoTrailing
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
  | RTFControlWord Text RTFControlWordEnd
  | RTFGroup [RTFContent]
  | RTFText Text
  deriving stock (Eq, Show, Generic)

instance TextShow RTFContent where showt = defaultShowt

$(makePrisms ''RTFContent)
$(makePrisms ''RTFControlWordEnd)

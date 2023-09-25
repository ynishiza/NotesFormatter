{-|
  See README for references
-}
module Notes.RTF.Types (
  rtfControlSymbol,
  getRtfControlSymbol,
  _SpaceSuffix,
  _RTFControlParam,
  _NoSuffix,
  --
  RTFContent (..),
  Word8,
  Text,
  RTFControlPrefix (..),
  RTFControlSuffix (..),
  -- Lens
  _RTFControlWord,
  _RTFControlSymbol,
  _RTFGroup,
  _RTFText,
  --
  charControl,
  charExtendedControlName,
  charSymbol,
  charReserved,
  charNewline,
  charOptionalDestination,
) where

import Data.Char (toUpper)
import Data.Word
import Notes.Utils

charControl :: Char
charControl = '\\'

charReserved :: String
charReserved = "\\{}"

charOptionalDestination :: Char
charOptionalDestination = '*'

{-|
  Officially, RTF names are all lower case.
  However, Apple's extended RTF may include upper case.

  e.g. \NeXTGraphic
-}
charExtendedControlName :: String
charExtendedControlName = charControlName <> ['A'..'Z']

charControlName :: String
charControlName = ['a' .. 'z'] 

charNewline :: String
charNewline = ['\n', '\r', '\f']

charNonSymbol :: String
charNonSymbol = ['0' .. '9'] <> [charOptionalDestination] <> charExtendedControlName <> (toUpper <$> charExtendedControlName)

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

rtfControlSymbol :: Char -> Either String RTFContent
rtfControlSymbol c =
  if c `elem` charSymbol
    then Right $ RTFControlSymbol c
    else Left $ "Invalid symbol " <> ['\'', c, '\'']

{-|
  RTF specs in the README
-}
data RTFContent
  = RTFControlSymbol Char
  | RTFControlWord RTFControlPrefix Text RTFControlSuffix
  | RTFGroup [RTFContent]
  | RTFText Text
  deriving stock (Eq, Show, Generic)

$(makePrisms ''RTFContent)
$(makePrisms ''RTFControlSuffix)

module RTF.ExtensionTypes (
  CocoaControl (..),
  ColorSpace (..),
  ExpandedColorTbl (..),
  CSValue,
  -- Lens
  csValueMax,
  _CSGray,
  _CSSRGB,
  _CSGenericRGB,
) where

import Utils

newtype CocoaControl = CocoaControl Text
  deriving stock (Eq, Show)

newtype ExpandedColorTbl = ExpandedColorTbl [Maybe ColorSpace]
  deriving stock (Eq, Show)

--
-- CS = Colorspace
-- Encoded of NSColorSpace: https://developer.apple.com/documentation/appkit/nscolorspace
--   by
--  {\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\cssrgb\c0\c0\c93333;\cssrgb\c20000\c20000\c20000; }
--
data ColorSpace
  = -- Device gray
    --  e.g. \csgray\c100000
    CSGray CSValue
  | -- sRGB
    --  e.g. \cssrgb\c20000\c20000\c20000;
    CSSRGB CSValue CSValue CSValue
  | -- GenericRGB
    --  e.g. csgenericrgb\c88766\c88766\c88766
    CSGenericRGB CSValue CSValue CSValue
  deriving stock (Eq, Show)

-- 0 ~ 100000
type CSValue = Int

csValueMax :: CSValue
csValueMax = 100000

$(makePrisms ''ColorSpace)

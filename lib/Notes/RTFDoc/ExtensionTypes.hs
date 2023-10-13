module Notes.RTFDoc.ExtensionTypes (
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

import Notes.Utils

data CocoaControl = CocoaControl Text (Maybe Int)
  deriving stock (Eq, Show, Generic)

newtype ExpandedColorTbl = ExpandedColorTbl [Maybe ColorSpace]
  deriving stock (Eq, Show, Generic)

{- |
  cs = Colorspace
  Encoding of NSColorSpace: https://developer.apple.com/documentation/appkit/nscolorspace
  by
    {\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\cssrgb\c0\c0\c93333;\cssrgb\c20000\c20000\c20000; }

  See README
-}
data ColorSpace
  = --
    -- Device gray
    --  e.g. \csgray\c100000
    CSGray CSValue
  | --
    -- sRGB
    --  e.g. 
    --    \cssrgb\c0\c0\c0                    ~ \red0\green0\blue0
    --    \cssrgb\c100000\c100000\c100000     ~ \red255\green255\blue255
    --    \cssrgb\c88766\c88766\c88766        ~ \red226\green226\blue226
    --    \cssrgb\c20000\c20000\c20000\c100;     with alpha
    --
    CSSRGB CSValue CSValue CSValue (Maybe CSValue)
  | --
    -- GenericRGB
    --  e.g. csgenericrgb\c88766\c88766\c88766
    CSGenericRGB CSValue CSValue CSValue (Maybe CSValue)
  deriving stock (Eq, Show, Generic)

-- 0 ~ 100000
type CSValue = Int

csValueMax :: CSValue
csValueMax = 100000

$(makePrisms ''ColorSpace)

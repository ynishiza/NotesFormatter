module ParserGen (
  genFontFamily,
  genRTFColor,
  genFontInfo,
  genColorSpace,
  genRTFHeader,
) where

import Hedgehog
import Hedgehog.Gen as G
import Hedgehog.Range as R
import RTF.Encoding

commonFont :: [Text]
commonFont =
  [ "Monaco"
  , "HelveticaNueue"
  , "HelveticaNueue-Bold"
  ]

genFontFamily :: Gen FontFamily
genFontFamily = enumBounded

genRTFColor :: Gen RTFColor
genRTFColor =
  G.frequency
    [ (8, RTFColor <$> G.maybe w <*> G.maybe w <*> G.maybe w)
    , (1, return $ RTFColor Nothing Nothing Nothing)
    ]
 where
  w = word8 constantBounded

genFontInfo :: Gen FontInfo
genFontInfo =
  FontInfo
    <$> genInt
    <*> genFontFamily
    <*> G.maybe genInt
    <*> name
 where
  genInt = int (linear 0 20)
  nameBase = G.text (linear 2 20) $ G.frequency [(9, alphaNum), (1, G.element "-_ ")]
  name = G.frequency $ (length commonFont, nameBase) : ((1,) . return <$> commonFont)

genCocoaControl :: Gen CocoaControl
genCocoaControl = CocoaControl <$> ((<>) <$> name <*> (showt <$> value))
 where
  name = element ["rtf", "textscaling"]
  value = int (linear 0 10)

genColorSpace :: Gen ColorSpace
genColorSpace =
  G.choice
    [ CSGray <$> value
    , CSSRGB <$> value <*> value <*> value
    , CSGenericRGB <$> value <*> value <*> value
    ]
 where
  value =
    frequency
      [ (10, int (R.constant 0 csValueMax))
      , (1, return csValueMax)
      , (1, return 0)
      ]

genRTFHeader :: Gen RTFHeader
genRTFHeader =
  RTFHeader
    <$> charset
    <*> list (linear 1 5) genCocoaControl
    <*> (FontTbl <$> list (linear 0 5) (G.maybe genFontInfo))
    <*> list (linear 0 10) color
 where
  charset = Ansi <$> int (linear 0 10)
  color = (,) <$> genRTFColor <*> G.maybe genColorSpace

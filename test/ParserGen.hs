module ParserGen (
  genFontFamily,
  genColorDef,
) where

import Hedgehog
import Hedgehog.Gen as G
import Hedgehog.Range as R
import RTF.Parser

genFontFamily :: Gen FontFamily
genFontFamily = enumBounded

genColorDef :: Gen ColorDef
genColorDef = ColorDef <$> w <*> w <*> w
 where
  w = G.maybe (word8 constantBounded)

genFontInfo :: Gen FontInfo
genFontInfo =
  FontInfo
    <$> i
    <*> genFontFamily
    <*> G.maybe i
    <*> name
 where
  i = int (linear 0 20)
  name = G.text (linear 2 20) $ G.frequency [(9, alphaNum), (1, G.constant ' ')]

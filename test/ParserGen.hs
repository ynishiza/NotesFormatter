module ParserGen (
  genFontFamily,
  genRTFColor,
  genFontInfo,
  genColorSpace,
  genRTFHeader,
  genRTFContents,
  genRTFNonTextContent,
) where

import Data.Char (isPrint)
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

genName :: Gen Text
genName = G.text (R.constant 1 32) (G.element charControlName)

genControlWord:: Gen RTFControlWord
genControlWord = genControlWordWithName genName

genControlSymbol :: Gen RTFControlSymbol
genControlSymbol = rtfControlSymbol <$> element charSymbol

genControlWordWithName :: Gen Text -> Gen RTFControlWord
genControlWordWithName n =
  RTFControlWord
    <$> n
    <*> choice
      [ RTFControlParam <$> int (linear 1 100)
      , return NoTrailing
      , return TrailingSpace
      ]

genFontInfo :: Gen FontInfo
genFontInfo =
  FontInfo
    <$> genInt
    <*> genFontFamily
    <*> G.maybe genInt
    <*> name
 where
  genInt = int (linear 0 20)
  nameBase = G.text (linear 2 20) $ G.frequency [(9, alphaNum), (1, G.element "-_")]
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


genRTFContents :: Gen [RTFContent]
genRTFContents = 
  list (linear 1 200) (choice [genRTFNonTextContent, plainText])
    <&> clean
  where
    plainText = RTFContentT . RTFText <$> G.text (R.constant 1 200) (G.filter isPlainChar unicodeAll)
    isPlainChar c = (c `notElem` ("\\{}" :: String)) && isPrint c

    clean (RTFContentT (RTFText t1) : RTFContentT (RTFText t2) : rest) = clean $ RTFContentT (RTFText $ t1 <> " " <> t2) : rest
    -- make sure text begins with a non-alphabet delimiter
    clean (RTFContentW (RTFControlWord n NoTrailing) : RTFContentT (RTFText t) : rest) = RTFContentW (RTFControlWord n NoTrailing) : clean (RTFContentT (RTFText $ "!" <> t) : rest)
    -- make sure text begins with a non-number delimiter
    clean (RTFContentW (RTFControlWord n p@(RTFControlParam _)) : RTFContentT (RTFText t) : rest) = RTFContentW (RTFControlWord n p) : clean (RTFContentT (RTFText $ "a" <> t) : rest)
    clean (x : xs) = x : clean xs
    clean [] = []

genRTFNonTextContent :: Gen RTFContent
genRTFNonTextContent =
  choice
    [ RTFContentW <$> genControlWord,
      RTFContentS <$> frequency [
        (1, rtfControlSymbol <$> element ['\\', '{', '}']),
        (9, genControlSymbol)
      ]
    ]

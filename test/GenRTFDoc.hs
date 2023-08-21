module GenRTFDoc (
  genFontFamily,
  genRTFColor,
  genFontInfo,
  genColorSpace,
  genRTFHeader,
  genRTFContents,
  genRTFNonTextContent,
  genRTFDoc,
) where

import Data.Char (isPrint)
import Hedgehog
import Hedgehog.Gen as G
import Hedgehog.Range as R
import Notes.RTFDoc

commonFont :: [Text]
commonFont =
  [ "Monaco"
  , "HelveticaNueue"
  , "HelveticaNueue-Bold"
  ]

genRTFDoc :: Gen RTFDoc 
genRTFDoc = RTFDoc <$> genRTFHeader <*> genRTFContents

genRTFHeader :: Gen RTFHeader
genRTFHeader =
  RTFHeader
    <$> charset
    <*> list (linear 1 10) genCocoaControl
    <*> (FontTbl <$> list (linear 0 20) (G.maybe genFontInfo))
    <*> list (linear 0 20) color
 where
  charset = Ansi <$> int (linear 0 10)
  color = (,) <$> genRTFColor <*> G.maybe genColorSpace

genRTFContents :: Gen [RTFContent]
genRTFContents =
  list (linear 1 200) (choice [genRTFNonTextContent, plainText])
    <&> clean
 where
  plainText = RTFText <$> G.text (R.constant 1 200) (G.filter isPlainChar unicodeAll)
  isPlainChar c = (c `notElem` charReserved) && isPrint c

  clean ((RTFText t1) : (RTFText t2) : rest) = clean $ RTFText (t1 <> " " <> t2) : rest
  -- make sure text begins with a non-alphabet delimiter
  clean ((RTFControlWord prefix n NoSuffix) : (RTFText t) : rest) = RTFControlWord prefix n NoSuffix : clean (RTFText ("!" <> t) : rest)
  -- make sure text begins with a non-number delimiter
  clean ((RTFControlWord prefix n p@(RTFControlParam _)) : (RTFText t) : rest) = RTFControlWord prefix n p : clean (RTFText ("a" <> t) : rest)
  clean (x : xs) = x : clean xs
  clean [] = []

genRTFNonTextContent :: Gen RTFContent
genRTFNonTextContent =
  choice
    [ genControlWord
    , frequency
        [ (1, rtfControlSymbol <$> element ['\\', '{', '}'])
        , (9, genControlSymbol)
        ]
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

genControlWord :: Gen RTFContent
genControlWord = genControlWordWithName genName

genControlSymbol :: Gen RTFContent
genControlSymbol = rtfControlSymbol <$> element charSymbol

genControlWordWithName :: Gen Text -> Gen RTFContent
genControlWordWithName n =
  RTFControlWord
    <$> prefix
    <*> n
    <*> suffix
 where
  prefix =
    choice
      [ return NoPrefix,
        return StarPrefix
      ]
  suffix =
    choice
      [ RTFControlParam <$> int (linear 1 100)
      , return NoSuffix
      , return SpaceSuffix
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
genCocoaControl = CocoaControl <$> name <*> G.maybe value
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
      [ (6, int (R.constant 0 csValueMax))
      , (2, return csValueMax)
      , (2, return 0)
      ]


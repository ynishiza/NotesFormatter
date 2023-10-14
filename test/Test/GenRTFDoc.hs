module Test.GenRTFDoc (
  genFontFamily,
  genRTFColor,
  genFontInfo,
  genColorSpace,
  genRTFHeader,
  genRTFElements,
  genRTFNonTextContent,
  genRTFDocContents,
  genRTFDoc,
  genEscapedSymbol,
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
genRTFDoc = RTFDoc <$> genRTFHeader <*> genRTFDocContents

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

genRTFDocContents :: Gen [RTFDocContent]
genRTFDocContents =
  recursive
    choice
    [ genRTFDocContensBase
    ]
    [subterm genRTFDocContensBase (\x -> [ContentGroup x])]

-- note: contents except a group
genRTFDocContensBase :: Gen [RTFDocContent]
genRTFDocContensBase =
  list
    (linear 1 200)
    ( choice
        [ nonText
        , rtfElementToDocContent <$> plainText
        , genEscapedSymbol
        ]
    )
    <&> cleanRTFDocContents
 where
  nonText = rtfElementToDocContent <$> G.filter nonEscapedSymbol genRTFNonTextContent
  nonEscapedSymbol (RTFControlSymbol c) = c /= '\''
  nonEscapedSymbol _ = True

cleanRTFDocContents :: [RTFDocContent] -> [RTFDocContent]
-- case: combine adjacent texts
cleanRTFDocContents ((ContentText t1) : (ContentText t2) : rest) = cleanRTFDocContents $ ContentText (t1 <> " " <> t2) : rest
-- case: make sure text begins with a non-alphabet delimiter
cleanRTFDocContents ((ContentControlWord prefix n NoSuffix) : (ContentText t) : rest) = ContentControlWord prefix n NoSuffix : cleanRTFDocContents (ContentText ("!" <> t) : rest)
-- case: make sure text begins with a non-number delimiter
cleanRTFDocContents ((ContentControlWord prefix n p@(RTFControlParam _)) : (ContentText t) : rest) = ContentControlWord prefix n p : cleanRTFDocContents (ContentText ("a" <> t) : rest)
cleanRTFDocContents (x : xs) = x : cleanRTFDocContents xs
cleanRTFDocContents [] = []

genEscapedSymbol :: Gen RTFDocContent
genEscapedSymbol = ContentEscapedSequence <$> word8 constantBounded

genRTFElements :: Gen [RTFElement]
genRTFElements =
  recursive
    choice
    [ genRTFElementsBase
    ]
    [ subterm genRTFElements (\x -> [RTFGroup x])
    ]

-- note: contents except a group
genRTFElementsBase :: Gen [RTFElement]
genRTFElementsBase =
  list (linear 1 200) (choice [genRTFNonTextContent, plainText])
    <&> cleanRTFElements

plainText :: Gen RTFElement
plainText = RTFText <$> G.text (R.constant 1 200) (G.filter isPlainChar unicodeAll)
 where
  isPlainChar c = (c `notElem` charReserved) && isPrint c

cleanRTFElements :: [RTFElement] -> [RTFElement]
-- case: combine adjacent texts
cleanRTFElements ((RTFText t1) : (RTFText t2) : rest) = cleanRTFElements $ RTFText (t1 <> " " <> t2) : rest
-- case: make sure text begins with a non-alphabet delimiter
cleanRTFElements ((RTFControlWord prefix n NoSuffix) : (RTFText t) : rest) = RTFControlWord prefix n NoSuffix : cleanRTFElements (RTFText ("!" <> t) : rest)
-- case: make sure text begins with a non-number delimiter
cleanRTFElements ((RTFControlWord prefix n p@(RTFControlParam _)) : (RTFText t) : rest) = RTFControlWord prefix n p : cleanRTFElements (RTFText ("a" <> t) : rest)
cleanRTFElements (x : xs) = x : cleanRTFElements xs
cleanRTFElements [] = []

genRTFNonTextContent :: Gen RTFElement
genRTFNonTextContent =
  choice
    [ genControlWord
    , frequency
        [ -- case: escaped key characters
          -- e.g.
          --      \rtf      '\' as a keyword
          --      \\        '\' escaped as a literal
          (1, Prelude.either error id . rtfControlSymbol <$> element ['\\', '{', '}'])
        , -- case: normal symbol
          (9, genControlSymbol)
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
genName = G.text (R.constant 1 32) (G.element charExtendedControlName)

genControlWord :: Gen RTFElement
genControlWord = genControlWordWithName genName

genControlSymbol :: Gen RTFElement
genControlSymbol = Prelude.either error id . rtfControlSymbol <$> element charSymbol

genControlWordWithName :: Gen Text -> Gen RTFElement
genControlWordWithName n =
  RTFControlWord
    <$> prefix
    <*> n
    <*> suffix
 where
  prefix =
    choice
      [ return NoPrefix
      , return StarPrefix
      ]
  suffix =
    choice
      [ RTFControlParam <$> int (linear (-100) 100)
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
    , CSSRGB <$> value <*> value <*> value <*> genMaybe alphaValue
    , CSGenericRGB <$> value <*> value <*> value <*> genMaybe alphaValue
    ]
 where
  value =
    frequency
      [ (6, int (R.constant 0 csValueMax))
      , (2, return csValueMax)
      , (2, return 0)
      ]
  alphaValue = int (R.constant 0 1000)
  -- note: by default maybe is
  --    Nothing:Just = 2:1+size
  -- i.e. mostly Justs
  -- If size 5, then
  --    Nothing:Just = 2:8 = 20% Nothing
  genMaybe = G.resize 5 . G.maybe

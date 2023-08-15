module ParserGen (
  genFontFamily,
  genRTFColor,
  genFontInfo,
  genColorSpace,
  genRTFHeader,
  genRTFContents,
  genRTFContents2,
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

genRTFContents :: Gen [RTFContent]
genRTFContents = do
  texts <- list (linearFrom 10 0 100) plainText
  let contents = clean . concat <$> traverse (\t -> (t :) <$> others) texts
      contentsEndingWithText = clean <$> ((++) <$> contents <*> ((: []) <$> plainText))
  choice
    [ (:) <$> genRTFNonTextContent <*> contents
    , (:) <$> genRTFNonTextContent <*> contentsEndingWithText
    , contents
    , contentsEndingWithText
    ]
 where
  others = list (linear 1 20) genRTFNonTextContent
  plainText = RTFPlainText <$> G.text (R.constant 1 100) (G.filter isPlainChar unicodeAll)
  isPlainChar c = (c `notElem` ("//{}" :: String)) && isPrint c

  clean (RTFTag n TrailingSymbol : RTFPlainText t : rest) = RTFTag n TrailingSymbol : RTFPlainText ("!" <> t) : rest
  clean (RTFTag n p@(RTFControlParam _) : RTFPlainText t : rest) = RTFTag n p : RTFPlainText ("a" <> t) : rest
  clean (x : xs) = x : clean xs
  clean [] = []

genRTFContents2 :: Gen [RTFContent]
genRTFContents2 =
  list (linear 1 200) (choice [genRTFNonTextContent, plainText])
    <&> clean
 where
  plainText = RTFPlainText <$> G.text (R.constant 1 200) (G.filter isPlainChar unicodeAll)
  isPlainChar c = (c `notElem` ("\\{}" :: String)) && isPrint c

  clean (RTFPlainText t1 : RTFPlainText t2 : rest) = clean $ RTFPlainText (t1 <> " " <> t2) : rest
  clean (RTFTag n TrailingSymbol : RTFPlainText t : rest) = RTFTag n TrailingSymbol : clean (RTFPlainText ("!" <> t) : rest)
  clean (RTFTag n p@(RTFControlParam _) : RTFPlainText t : rest) = RTFTag n p : clean (RTFPlainText ("a" <> t) : rest)
  clean (x : xs) = x : clean xs
  clean [] = []

genRTFNonTextContent :: Gen RTFContent
genRTFNonTextContent =
  choice
    [ return RTFNewLine
    , return RTFLiteralCloseBrace
    , return RTFLiteralOpenBrace
    , return RTFLiteralSlash
    , RTFTag
        <$> G.text (R.constant 1 20) alpha
        <*> choice
          [ RTFControlParam <$> word8 (linear 1 100)
          , return TrailingSymbol
          , return TrailingSpace
          ]
    ]

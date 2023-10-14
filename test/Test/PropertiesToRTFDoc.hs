module Test.PropertiesToRTFDoc (
  properties,
  prop_rtfheader,
) where

import Control.Lens
import Data.Foldable
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import GHC.Exts (fromString)
import Hedgehog
import Notes.RTFDoc
import Notes.RTFDoc.RawParse
import Test.GenRTFDoc
import Text.Megaparsec qualified as M

testCount :: TestLimit
testCount = 200

property_ :: PropertyT IO () -> Property
property_ = withTests testCount . property

labelName :: Show a => a -> LabelName
labelName = fromString . show

isEqualToParseable :: forall a. (Eq a, Show a, Renderable a, ToRTFDoc a, Parseable a) => a -> PropertyT IO ()
isEqualToParseable x = do
  let text = render x
  annotate $ T.unpack text
  case M.parse (parse @a) "" text of
    Left e -> fail $ show e
    Right v -> case parseDoc_ (toRTFDoc @a) text of
      Left msg -> fail msg
      Right v' -> v === v'

testEquality :: forall a. (Eq a, Show a, Renderable a, ToRTFDoc a) => a -> PropertyT IO ()
testEquality x = do
  tripping x render (parseDoc_ toRTFDoc)

properties :: Group
properties = $$discover

prop_fontfamily :: Property
prop_fontfamily = property_ $ do
  x <- forAll genFontFamily
  coverEnum x
  testEquality x
  isEqualToParseable x

prop_fontInfo :: Property
prop_fontInfo = property_ $ do
  x@(FontInfo _ fontFamily charset _) <- forAll genFontInfo
  coverEnum fontFamily
  cover 1 "No charset" $ isNothing charset
  cover 1 "Has charset" $ isJust charset
  testEquality x
  isEqualToParseable x

prop_rtfColor :: Property
prop_rtfColor = property_ $ do
  x <- forAll genRTFColor
  testEquality x
  isEqualToParseable x

prop_colorSpace :: Property
prop_colorSpace = property_ $ do
  x <- forAll genColorSpace
  let checkCoverage (name, lns) = do
        cover 1 (name <> " == 0") $ fromMaybe False $ previews lns (== 0) x
        cover 1 (name <> " == max") $ fromMaybe False $ previews lns (== csValueMax) x
        cover 1 (name <> " > 0 && < max") $ fromMaybe False $ previews lns (\v -> v > 0 && v < csValueMax) x

  traverse_
    checkCoverage
    [ ("CSGray", _CSGray)
    , ("CSSRGB R", _CSSRGB . _1)
    , ("CSSRGB G", _CSSRGB . _2)
    , ("CSSRGB B", _CSSRGB . _3)
    , ("CSGenericRGB R", _CSGenericRGB . _1)
    , ("CSGenericRGB G", _CSGenericRGB . _2)
    , ("CSGenericRGB B", _CSGenericRGB . _3)
    ]

  cover 2 "CSSRGB without alpha" $ has (_CSSRGB . _4 . _Nothing) x
  cover 2 "CSSRGB with alpha" $ has (_CSSRGB . _4 . _Just) x
  cover 2 "CSGenericRGB without alpha" $ has (_CSGenericRGB . _4 . _Nothing) x
  cover 2 "CSGenericRGB with alpha" $ has (_CSGenericRGB . _4 . _Just) x

  testEquality x
  isEqualToParseable x

prop_rtfheader :: Property
prop_rtfheader = property_ $ do
  x@(RTFHeader _ _ _ colors) <- forAll genRTFHeader
  -- Color
  cover 1 "Default color " $ any (\(RTFColor{..}, _) -> isNothing red && isNothing green && isNothing green) colors
  cover 10 "Non-default color " $ any (\(RTFColor{..}, _) -> isJust red || isJust green || isJust green) colors
  cover 1 "Default color space" $ any (isNothing . snd) colors
  cover 10 "Non-default color space" $ any (isJust . snd) colors
  cover 1 "Many colors" $ length colors > 5
  cover 1 "One color" $ length colors == 1

  -- Font
  cover 1 "default font" $ not $ null $ toListOf (_rtfFontTbl . _FontTbl . each . _Nothing) x
  cover 1 "non default font" $ not $ null $ toListOf (_rtfFontTbl . _FontTbl . each . _Just) x

  isEqualToParseable x
  testEquality x

prop_escapedSymbol :: Property
prop_escapedSymbol = property_ $ do
  symbol@(ContentEscapedSequence _) <- forAll genEscapedSymbol
  content <- forAll genRTFDocContents
  let symbolText = render symbol
      contentText = render content
      allText = render (symbol : content)

  -- test: must always be fixed length
  -- In particular, should pad 1 digit numbers
  -- e.g.
  --    GOOD    \'01
  --    BAD     \'1
  T.length symbolText === 4
  T.length allText === (T.length symbolText + T.length contentText)

  -- test: first digit
  traverse_
    ( \c ->
        cover
          1
          (fromString $ c : "*")
          $ T.unpack symbolText !! 2 == c
    )
    ("0123456789abcdef" :: String)
  -- test: second digit
  traverse_
    ( \c ->
        cover
          1
          (fromString $ "*" <> [c])
          $ T.unpack symbolText !! 3 == c
    )
    ("0123456789abcdef" :: String)

prop_rtfdocContent :: Property
prop_rtfdocContent = property_ $ do
  content <- forAll genRTFDocContents
  testEquality content

prop_rtfdoc :: Property
prop_rtfdoc = property_ $ do
  d@(RTFDoc _ contents) <- forAll genRTFDoc
  let fullText = T.intercalate "" $ toListOf (traverse . _ContentText) contents

  cover 1 "long text (> 100 chars)" $ T.length fullText > 100
  cover 1 "short text (< 10 chars)" $ T.length fullText < 10
  cover 20 "ContentControlWord" $ has (traverse . _ContentControlWord) contents
  cover 20 "ContentControlSymbol" $ has (traverse . _ContentControlSymbol) contents
  cover 20 "ContentEscapedSequence" $ has (traverse . _ContentEscapedSequence) contents
  cover 20 "ContentGroup" $ has (traverse . _ContentGroup) contents

  -- test: escaping key characters
  -- i.e. some characters have special meaning so they need to be escaped in order
  -- e.g.
  --      \rtf      '\' as a keyword
  --      \\        '\' escaped as a literal
  traverse_
    ( \c ->
        cover
          1
          (fromString $ "escaped key character:" <> [c])
          $ any (\v -> preview _ContentControlSymbol v == Just c) contents
    )
    ("\\{}" :: String)
  testEquality d

coverEnum :: (MonadTest f, Show a, Eq a, Bounded a, Enum a) => a -> f ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover = realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

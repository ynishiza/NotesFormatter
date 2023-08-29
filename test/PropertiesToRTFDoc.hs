module PropertiesToRTFDoc (
  properties,
) where

import Control.Lens
import Data.Foldable
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import GHC.Exts (fromString)
import GenRTFDoc
import Hedgehog
import Notes.RTFDoc
import Notes.RTFDoc.RawParse
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

  testEquality x
  isEqualToParseable x

prop_rtfdoc :: Property
prop_rtfdoc = property_ $ do
  (RTFDoc _ contents) <- forAll genRTFDoc
  let fullText = T.intercalate "" $ toListOf (traverse . _RTFText) contents
  cover 1 "long text" $ T.length fullText > 100
  cover 1 "short text" $ T.length fullText < 10
  -- testEquality x
  -- isEqualToParseable x

coverEnum :: (MonadTest f, Show a, Eq a, Bounded a, Enum a) => a -> f ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover = realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

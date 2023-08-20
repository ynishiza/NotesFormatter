module PropertiesRawParse (properties) where

import Control.Lens
import Data.Attoparsec.ByteString hiding (parse)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Exts (fromString)
import Hedgehog
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import GenRTFDoc
import RTFDoc
import RTFDoc.RawParse

testCount :: TestLimit
testCount = 200

properties :: Group
properties = $$discover

labelName :: Show a => a -> LabelName
labelName = fromString . show

property_ :: PropertyT IO () -> Property
property_ = withTests testCount . property

prop_fontFamily :: Property
prop_fontFamily = property_ $ do
  x <- forAll genFontFamily
  coverEnum x
  testRTFEncoding x

prop_colorDef :: Property
prop_colorDef = property_ $ do
  x@(RTFColor r g b) <- forAll genRTFColor
  cover 10 "small red" $ maybe False (< 100) r
  cover 10 "large red" $ maybe False (> 200) r
  cover 1 "default red" $ isNothing r
  cover 10 "small green" $ maybe False (< 100) g
  cover 10 "large green" $ maybe False (> 200) g
  cover 1 "default green" $ isNothing g
  cover 10 "small blue" $ maybe False (< 100) b
  cover 10 "large blue" $ maybe False (> 200) b
  cover 1 "default blue" $ isNothing b
  cover 1 "all default" $ isNothing r && isNothing g && isNothing b
  cover 1 "all non-default" $ isJust r && isJust g && isJust b
  testRTFEncoding x

prop_fontInfo :: Property
prop_fontInfo = property_ $ do
  x@(FontInfo fnum fontFamily _ fname) <- forAll genFontInfo
  coverEnum fontFamily
  testRTFEncoding x

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

  testRTFEncoding x

prop_rtfHeader :: Property
prop_rtfHeader = property_ $ do
  x@(RTFHeader _ _ _ colors) <- forAll genRTFHeader
  cover 1 "Default color " $ any (\(RTFColor{..}, _) -> isNothing red && isNothing green && isNothing green) colors
  cover 10 "Non-default color " $ any (\(RTFColor{..}, _) -> isJust red || isJust green || isJust green) colors
  cover 1 "Default color space" $ any (isNothing . snd) colors
  cover 10 "Non-default color space" $ any (isJust . snd) colors
  testRTFEncoding x

prop_rtfContentOther :: Property
prop_rtfContentOther = property_ $ do
  x <- forAll $ G.list (R.linearFrom 5 0 100) genRTFNonTextContent
  -- TODO: plaintext
  -- collect $ T.intercalate "a" $ render <$> x
  tripping x (T.intercalate "" . (renderRTFContent <$>)) (parseOnly (many' parseRTFContent) . T.encodeUtf8)
  -- collect $ parseOnly (many' (decodeRTF @RTFContent)) $ T.encodeUtf8 $ T.intercalate "" $ render <$> x

prop_rtfContent :: Property
prop_rtfContent = property_ $ do
  x <- forAll genRTFContents

  -- cover 20 "Newline" $ has (traverse . _RTFNewLine) x
  -- cover 20 "Literal \\" $ has (traverse . _RTFLiteralSlash) x
  -- cover 20 "Literal {" $ has (traverse . _RTFLiteralOpenBrace) x
  -- let f = to (\(RTFControlWord _ v) -> v)
  cover 20 "Tag with trailing space" $ has (traverse . _RTFControlWord . _2 . _TrailingSpace) x
  cover 20 "Tag with trailing symbol" $ has (traverse . _RTFControlWord . _2 . _NoTrailing) x
  cover 20 "Tag with parameter" $ has (traverse . _RTFControlWord . _2 . _RTFControlParam) x
  -- cover 20 "Large text" $ anyOf (traverse . _RTFPlainText) (\t -> T.length t > 100) x
  tripping x (T.intercalate "" . (renderRTFContent <$>)) (parseOnly (many' parseRTFContent) . T.encodeUtf8)

-- undefined

testRTFEncoding :: (Show a, Eq a, Renderable a, Parseable a, Monad m) => a -> PropertyT m ()
testRTFEncoding x = tripping x render (parseOnly parse . T.encodeUtf8)

coverEnum :: (Monad m, Show a, Enum a, Eq a, Bounded a) => a -> PropertyT m ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover = realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

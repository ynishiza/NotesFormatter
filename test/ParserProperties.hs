{-# LANGUAGE AllowAmbiguousTypes #-}

module ParserProperties (group) where

import Data.Attoparsec.ByteString
import Data.Foldable
import Data.Maybe (isJust, isNothing)
import Data.Text.Encoding qualified as T
import GHC.Exts (fromString)
import Hedgehog
import ParserGen
import RTF.Parser

group :: Group
group = $$discover

labelName :: Show a => a -> LabelName
labelName = fromString . show

property_ :: PropertyT IO () -> Property
property_ = withTests 300 . property

prop_fontFamily :: Property
prop_fontFamily = property_ $ do
  x <- forAll genFontFamily
  coverEnum x
  testRTFEncoding x

prop_colorDef :: Property
prop_colorDef = property_ $ do
  x@(ColorDef r g b) <- forAll genColorDef
  cover 10 "small red" $ maybe False (< 100) r
  cover 10 "large red" $ maybe False (> 200) r
  cover 1 "default red" $ isNothing r
  cover 10 "small green" $ maybe False (< 100) g
  cover 10 "large green" $ maybe False (> 200) g
  cover 1 "default green" $ isNothing g
  cover 10 "small blue" $ maybe False (< 100) b
  cover 10 "large blue" $ maybe False (> 200) b
  cover 1 "default blue" $ isNothing b
  cover 0.01 "all default" $ isNothing r && isNothing g && isNothing b
  cover 0.01 "all non-default" $ isJust r && isJust g && isJust b
  testRTFEncoding x

testRTFEncoding :: (Show a, Eq a, RTFEncoding a, Monad m) => a -> PropertyT m ()
testRTFEncoding x = tripping x encodeRTF (parseOnly decodeRTF . T.encodeUtf8)

coverEnum :: (Monad m, Show a, Enum a, Eq a, Bounded a) => a -> PropertyT m ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover = realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

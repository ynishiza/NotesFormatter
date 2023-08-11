{-# LANGUAGE AllowAmbiguousTypes #-}

module ParserProperties (group) where

import Data.Attoparsec.ByteString
import Data.Foldable
import Data.Text.Encoding qualified as T
import GHC.Exts (fromString)
import Hedgehog
import Hedgehog.Gen
import RTF.Parser

genFontFamily :: Gen FontFamily
genFontFamily = enumBounded

group :: Group
group = $$discover

labelName :: Show a => a -> LabelName
labelName = fromString . show

property_ :: PropertyT IO () -> Property
property_ = withTests 200 . property

prop_fontFamily :: Property
prop_fontFamily = property_ $ do
  x <- forAll genFontFamily
  coverEnum x
  tripping x encodeRTF (parseOnly decodeRTF . T.encodeUtf8)

coverEnum :: (Monad m, Show a, Enum a, Eq a, Bounded a) => a -> PropertyT m ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover =  realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

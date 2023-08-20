module RTFDocParserProperties (
  rtfDocParseProperties,
) where

import Data.Attoparsec.ByteString
import Data.Text.Encoding
import Hedgehog
import ParserGen
import RTFDoc.Encoding
import RTFDoc.ToRTF
import TestUtils

testCount :: TestLimit
testCount = 200

property_ :: PropertyT IO () -> Property
property_ = withTests testCount . property

testEquality :: forall a. (Eq a, Show a, ToRTF a, RTFEncoding a) => Gen a -> Property
testEquality gen = property_ $ do
  x <- forAll gen
  let text = encodeUtf8 $ encodeRTF x
  -- collect text
  -- tripping x (encodeUtf8 . encodeRTF) f 
  tripping x (encodeUtf8 . encodeRTF) f 
    where 
      f :: ByteString -> Either String a
      f b = case parseDoc (toRTF @a) b of
              Left e -> Left $ show e
              Right (v, _) -> Right v
  -- case parseOnly (decodeRTF @a) text of
  --   Left e -> fail e
  --   Right v -> case parseDoc (toRTF @a) text of
  --     Left e -> fail $ show e
  --     Right (v', _) -> v === v'

rtfDocParseProperties :: Group
rtfDocParseProperties = $$discover

prop_fontfamily :: Property
prop_fontfamily = testEquality genFontFamily

prop_rtfColor :: Property
prop_rtfColor = testEquality genRTFColor

prop_colorSpace :: Property
prop_colorSpace = testEquality genColorSpace

prop_header :: Property
prop_header = testEquality genRTFHeader

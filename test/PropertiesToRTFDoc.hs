module PropertiesToRTFDoc
  (
  properties,
) where

-- import Data.Attoparsec.ByteString
import Data.Text.Encoding
import Hedgehog
import GenRTFDoc
import RTFDoc.RawParse
import RTFDoc
-- import TestUtils

testCount :: TestLimit
testCount = 200

property_ :: PropertyT IO () -> Property
property_ = withTests testCount . property

testEquality :: forall a. (Eq a, Show a, Renderable a, ToRTFDoc a) => Gen a -> Property
testEquality gen = property_ $ do
  x <- forAll gen
  -- let text = encodeUtf8 $ encodeRTF x
  -- collect text
  -- tripping x (encodeUtf8 . encodeRTF) f 
  tripping x (encodeUtf8 . render) parseRTF 
    where 
      parseRTF :: ByteString -> Either String a
      parseRTF b = case parseDoc (toRTFDoc @a) b of
              Left e -> Left $ show e
              Right (v, _) -> Right v
  -- case parseOnly (decodeRTF @a) text of
  --   Left e -> fail e
  --   Right v -> case parseDoc (toRTF @a) text of
  --     Left e -> fail $ show e
  --     Right (v', _) -> v === v'

properties :: Group
properties = $$discover

prop_fontfamily :: Property
prop_fontfamily = testEquality genFontFamily

prop_rtfColor :: Property
prop_rtfColor = testEquality genRTFColor

prop_colorSpace :: Property
prop_colorSpace = testEquality genColorSpace

prop_rtfheader :: Property
prop_rtfheader = testEquality genRTFHeader

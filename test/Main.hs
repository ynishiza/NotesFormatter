module Main (main) where

import Test.Hspec
import Test.PropertiesRawParse qualified
import Test.PropertiesToRTFDoc qualified
import Test.Spec qualified
import Test.SpecRTFParser qualified
import Test.SpecSamples qualified
import Test.SpecApp qualified
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specTree <- testSpecs $ describe "main" $ do
    Test.Spec.spec
    Test.SpecRTFParser.spec
    Test.SpecSamples.spec
    Test.SpecApp.spec

  defaultMain $
    testGroup "main" $
      [ fromGroup Test.PropertiesRawParse.properties
      , fromGroup Test.PropertiesToRTFDoc.properties
      ]
        <> specTree

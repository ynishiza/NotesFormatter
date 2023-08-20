module Main (main) where

import PropertiesRawParse qualified
import PropertiesToRTFDoc qualified
import Spec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specTree <- testSpecs spec
  defaultMain $
    testGroup "main" $
      [ fromGroup PropertiesRawParse.properties
      , fromGroup PropertiesToRTFDoc.properties
      ]
        <> specTree

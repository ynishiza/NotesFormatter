module Main (main) where

import ParserProperties
import RTFDocParserProperties
import Spec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specTree <- testSpecs spec
  defaultMain $
    testGroup "main" $
      [fromGroup group, fromGroup rtfDocParseProperties] <> specTree

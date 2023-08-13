module Main (main) where

import Test.Tasty.Hedgehog
import Test.Tasty
import ParserProperties

main :: IO ()
main = defaultMain $ fromGroup group

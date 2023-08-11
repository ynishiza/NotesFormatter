{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import qualified B.Lib
import Test.Hspec

main :: IO ()
main = hspec $ describe "App" $ do
  it "Hello" $ do
    B.Lib.message `shouldBe` "Hello"

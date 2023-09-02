{-# LANGUAGE QuasiQuotes #-}

module Test.SpecRTFParser (
  spec,
) where

import Control.Monad.Combinators
import Data.Text qualified as T
import Notes.Process
import Notes.RTF.ContentParser
import Notes.RTFDoc
import Test.Hspec hiding (runIO)
import Test.Utils

spec :: Spec
spec = describe "RTF Parsers" $ do
  let testError :: ContentParser p -> Text -> Text -> Expectation
      testError p b expected = case parseDoc_ p b of
        Left e -> do
          T.strip (T.pack e) `shouldBe` T.strip expected
        Right _ -> expectationFailure $ "Failed to test: " <> T.unpack expected

  describe "RTF.ContentParser" $ do
    describe "RTFControlWord" $ do
      let pNoParam = rtfControlWordLabel_ "a"
      let pParam = rtfControlWordValue_ "a" (\x -> if x == 1 then Just x else Nothing)

      it "[error message] wrong word parsing a word with no parameter" $ do
        testError
          pNoParam
          "\\b abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" SpaceSuffix
expecting RTFControlWord a
|]

        testError
          pNoParam
          "\\b1"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlWord b
|]

      it "[error message] wrong word parsing a word with parameter" $ do
        testError
          pParam
          "\\a abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" SpaceSuffix
expecting RTFControlWord * a (RTFControlParam *)
|]

        testError
          pParam
          "\\b1 abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1),RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlSymbol a|]

      it "[error message] not a word" $ do
        testError
          pNoParam
          "\\& abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '&',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '&'
expecting RTFControlWord
|]

      it "[error message] error location in a sequence" $ do
        testError
          (count 3 pNoParam)
          "\\a\\a\\b\\c"
          [multiline|
RTF:1:75:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord a
|]

        testError
          (count 3 pParam)
          "\\a1\\a1\\b\\c"
          [multiline|
RTF:1:97:
  |
1 | RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord * b (RTFControlParam *)
|]

    describe "RTFControlSymbol" $ do
      let pAtMark = rtfSymbol_ '#'

      it "[error message] wrong symbol" $ do
        -- case: wrong symbol
        testError
          pAtMark
          "\\@ abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '@',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

      it "[error message] not a symbol" $ do
        -- case: wrong type
        testError
          pAtMark
          "\\a abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" SpaceSuffix
expecting RTFControlSymbol
|]

      it "[error message] location in a sequence" $ do
        testError
          (count 10 pAtMark)
          "\\#\\#\\@ abc"
          [multiline|
RTF:1:43:
  |
1 | RTFControlSymbol '#',RTFControlSymbol '#',RTFControlSymbol '@',RTFText " abc"
  |                                           ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

    describe "RTFGroup" $ do
      let pGroup = rtfGroup "Group" (rtfSymbol_ '&' >> rtfControlWordLabel_ "abc" >> rtfText_ "hello")

      it "[error message] wrong group content" $ do
        testError
          pGroup
          "{\\#}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '#']
  | ^

RTFGroup [RTFControlSymbol '#']:1:1:
  |
1 | RTFControlSymbol '#'
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '#'
expecting RTFControlSymbol '&'
|]

        testError
          pGroup
          "{\\& a}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFText " a"]
  | ^

RTFGroup [RTFControlSymbol '&',RTFText " a"]:1:22:
  |
1 | RTFControlSymbol '&',RTFText " a"
  |                      ^^^^^^^^^^^^
unexpected RTFText " a"
expecting RTFControlWord
|]

        testError
          pGroup
          "{\\&\\abc abc}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"]
  | ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"]:1:64:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"
  |                                                                ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFText "hello"
|]

        testError
          pGroup
          "{\\&\\abc hello\\#}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#']
  | ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#']:1:80:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#'
  |                                                                                ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '#'
expecting end of input
|]
        testError
          (pGroup >> rtfText_ "def")
          "{\\&\\abc hello}abc"
          [multiline|
RTF:1:91:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFText "abc"
  |                                                                                           ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFText "def"
|]

      it "[error message] not a group" $ do
        testError
          pGroup
          "\\a"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" NoSuffix
expecting RTFGroup
|]

      it "[error message] location in a sequence" $ do
        testError
          (count 3 pGroup)
          "{\\&\\abc hello}{\\&\\abc hello}{\\&\\d} abc"
          [multiline|
RTF:1:181:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix],RTFText " abc"
  |                                                                                                                                                                                     ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix]:1:22:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix
  |                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "d" NoSuffix
expecting RTFControlWord abc
|]

    describe "RTFText" $ do
      let pHello = rtfText_ "Hello"

      it "[error message] not a word" $ do 
        testError
          (count 3 pHello)
          "\\abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "abc" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "abc" NoSuffix
expecting RTFText
|]

      it "[error message] wrong text" $ do 
        testError
          (count 3 pHello)
          "This is a pen"
          [multiline|
RTF:1:1:
  |
1 | RTFText "This is a pen"
  | ^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFText "This is a pen"
expecting RTFText "Hello"
|]

  describe "RTFDoc" $ do
    describe "ColorSpace" $ do
      it "[error message] not a ColorSpace" $ do
        testError
          (toRTFDoc @ColorSpace)
          "\\abc\\abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "abc" NoSuffix,RTFControlWord NoPrefix "abc" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "abc" NoSuffix
expecting RTFControlWord csgenericrgb, RTFControlWord csgray, or RTFControlWord cssrgb|]

      it "[error message] wrong content" $ do
        testError
          (toRTFDoc @ColorSpace)
          "\\csgray\\abc"
          [multiline|
RTF:1:43:
  |
1 | RTFControlWord NoPrefix "csgray" NoSuffix,RTFControlWord NoPrefix "abc" NoSuffix
  |                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "abc" NoSuffix
expecting RTFControlWord * abc (RTFControlParam *)
|]

    describe "ColorTbl" $ do
      it "[error message] not a ColorTbl" $ do
        testError
          (toRTFDoc @ColorTbl)
          "\\csgray"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "csgray" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "csgray" NoSuffix
expecting RTFGroup
|]

      it "[error message] wrong content" $ do
        testError
          (toRTFDoc @ColorTbl)
          "{\\cg\\red1;\\green0;\\blue3;;}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlWord NoPrefix "cg" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";;"]
  | ^

RTFGroup [RTFControlWord NoPrefix "cg" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"]:1:1:
  |
1 | RTFControlWord NoPrefix "cg" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "cg" NoSuffix
expecting RTFControlWord colortbl
|]
        testError
          (toRTFDoc @ColorTbl)
          "{\\colortbl\\rd1;\\green0;\\blue3;;}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "rd" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";;"]
  | ^

RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "rd" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"]:1:45:
  |
1 | RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "rd" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 0),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"
  |                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "rd" (RTFControlParam 1)
expecting RTFControlSymbol blue, RTFControlSymbol green, RTFControlSymbol red, RTFText, or end of input
|]

        testError
          (toRTFDoc @ColorTbl)
          "{\\colortbl\\red1;\\green;\\blue3;;}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";;"]
  | ^

RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"]:1:107:
  |
1 | RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";",RTFText ";"
  |                                                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "green" NoSuffix
expecting RTFControlWord * green (RTFControlParam *), RTFText, or end of input
|]

        testError
          (toRTFDoc @ColorTbl)
          "{\\colortbl\\red1;\\green1;\\lue3;;}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "lue" (RTFControlParam 3),RTFText ";;"]
  | ^

RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "lue" (RTFControlParam 3),RTFText ";",RTFText ";"]:1:171:
  |
1 | RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "lue" (RTFControlParam 3),RTFText ";",RTFText ";"
  |                                                                                                                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "lue" (RTFControlParam 3)
expecting RTFControlSymbol blue, RTFControlSymbol green, RTFControlSymbol red, RTFText, or end of input
|]

        testError
          (toRTFDoc @ColorTbl)
          "{\\colortbl\\red1;\\green;\\blue3}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3)]
  | ^

RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3)]:1:107:
  |
1 | RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" NoSuffix,RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3)
  |                                                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "green" NoSuffix
expecting RTFControlWord * green (RTFControlParam *), RTFText, or end of input
|]

        testError
          (count 3 $ toRTFDoc @ColorTbl)
          "{\\colortbl\\red1;\\green1;\\blue3;}{\\colortbl\\red1;\\green1;\\blue3;}a"
          [multiline|
RTF:1:489:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";"],RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";"],RTFText "a"
  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ^^^^^^^^^^^
unexpected RTFText "a"
expecting RTFGroup
|]

        testError
          (count 3 $ toRTFDoc @ColorTbl)
          "{\\colortbl\\red1;\\green1;\\blue3;}{\\colortbl\\red1;\\green1;\\blue3;}{\\colortbl abc;}"
          [multiline|
RTF:1:489:
  |
1 | RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";"],RTFGroup [RTFControlWord NoPrefix "colortbl" NoSuffix,RTFControlWord NoPrefix "red" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "green" (RTFControlParam 1),RTFText ";",RTFControlWord NoPrefix "blue" (RTFControlParam 3),RTFText ";"],RTFGroup [RTFControlWord NoPrefix "colortbl" SpaceSuffix,RTFText "abc;"]
  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ^

RTFGroup [RTFControlWord NoPrefix "colortbl" SpaceSuffix,RTFText "abc",RTFText ";"]:1:48:
  |
1 | RTFControlWord NoPrefix "colortbl" SpaceSuffix,RTFText "abc",RTFText ";"
  |                                                ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFControlWord, RTFText ";", or end of input
|]
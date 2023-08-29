{-# LANGUAGE QuasiQuotes #-}

module ParserSpec (
  spec,
) where

import Control.Monad.Combinators
import Data.Text qualified as T
import Notes.Process
import Notes.RTF.ContentParser
import Notes.RTFDoc
import Test.Hspec hiding (runIO)
import TestUtils

spec :: Spec
spec = describe "Parser" $ do
  let testError :: ContentParser p -> Text -> Text -> Expectation
      testError p b expected = case parseDoc_ p b of
        Left e -> do
          T.strip (T.pack e) `shouldBe` T.strip expected
        Right _ -> expectationFailure $ "Failed to test: " <> T.unpack expected

  describe "Base" $ do
    it "[RTFControlWord] no parameter" $ do
      let p = rtfControlWordLabel_ "a"
      testError
        p
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
        p
        "\\b1"
        [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlWord b
|]
      testError
        p
        "\\& abc"
        [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '&',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '&'
expecting RTFControlWord
|]

      testError
        (count 3 p)
        "\\a\\a\\b\\c"
        [multiline|
RTF:1:75:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord a
|]

    it "[RTFControlWord] with parameter" $ do
      let p = rtfControlWordValue_ "a" (\x -> if x == 1 then Just x else Nothing)
      testError
        p
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
        p
        "\\b1 abc"
        [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1),RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlSymbol a|]

      testError
        (count 3 p)
        "\\a1\\a1\\b\\c"
        [multiline|
RTF:1:97:
  |
1 | RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord * b (RTFControlParam *)
|]

    it "[RTFControlSymbol]" $ do
      let p = rtfSymbol_ '#'
      -- case: wrong symbol
      testError
        p
        "\\@ abc"
        [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '@',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

      -- case: wrong type
      testError
        p
        "\\a abc"
        [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" SpaceSuffix
expecting RTFControlSymbol
|]

      testError
        (count 10 p)
        "\\#\\#\\@ abc"
        [multiline|
RTF:1:43:
  |
1 | RTFControlSymbol '#',RTFControlSymbol '#',RTFControlSymbol '@',RTFText " abc"
  |                                           ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

    it "[RTFGroup]" $ do
      let p = rtfGroup "Group" (rtfSymbol_ '&' >> rtfControlWordLabel_ "abc" >> rtfText_ "hello")
      testError
        p
        "\\a"
        [multiline|
 RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" NoSuffix
expecting RTFGroup
|]

      testError
        p
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
        p
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
        p
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
        p
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
        (p >> rtfText_ "def")
        "{\\&\\abc hello}abc"
        [multiline|
RTF:1:91:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFText "abc"
  |                                                                                           ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFText "def"
|]

      testError
        (count 3 p)
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

  it "ColorSpace" $ do
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

  it "[ColorTbl]" $ do
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

{-# LANGUAGE QuasiQuotes #-}

module Test.SpecRTFParser (
  spec,
) where

import Control.Monad.Combinators
import Data.Text qualified as T
import Notes.Process
import Notes.RTFDoc
import RTF.ElementParser
import Test.Hspec hiding (runIO)

spec :: Spec
spec = describe "RTF Parsers" $ do
  let testError :: (HasCallStack) => ElementParser p -> Text -> Text -> Expectation
      testError p b expected = case parseDoc_ p b of
        Left e -> do
          T.strip (T.pack e) `shouldBe` T.strip expected
        Right _ -> expectationFailure $ "Failed to test: " <> T.unpack expected

  describe "RTFDoc" $ do
    describe "RTFDocContent" $ do
      it "[error message] " $ do
        -- case: non-hex text
        testError
          (toRTFDoc @RTFDocContent)
          "\\'y"
          [multiline|
RTF:1:23:
  |
1 | RTFControlSymbol '\'',RTFText "y"
  |                       ^
ContentEscapedSequence: y is not a valid hex
|]

        -- case: too short
        testError
          (toRTFDoc @RTFDocContent)
          "\\'a"
          [multiline|
RTF:1:23:
  |
1 | RTFControlSymbol '\'',RTFText "a"
  |                       ^
ContentEscapedSequence: a is not a valid hex
|]
        -- case: too short
        testError
          (toRTFDoc @RTFDocContent)
          "\\'\\abc"
          [multiline|
RTF:1:23:
  |
1 | RTFControlSymbol '\'',RTFControlWord NoPrefix "abc" NoSuffix
  |                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "abc" NoSuffix
expecting ContentEscapedSequence: hex text
|]

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

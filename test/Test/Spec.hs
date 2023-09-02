{-# LANGUAGE QuasiQuotes #-}

module Test.Spec (
  spec,
) where

import Control.Lens
import Data.ByteString.Char8 qualified as B
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Notes.Config
import Notes.Process
import Notes.RTFDoc
import Test.Hspec hiding (runIO)
import Test.Utils

spec :: Spec
spec = describe "main" $ do
  rtfSpec

  describe "Config" $ do
    let testJSONParse :: forall a. (Show a, Eq a, FromJSON a) => a -> Text -> Expectation
        testJSONParse expected text = do
          let result = eitherDecode' @a $ B.fromStrict $ encodeUtf8 text
          result `shouldBe` Right expected

    it "should parse Config" $ do
      testJSONParse
        ( Config
            { cfgColorMap = [ColorMap (RTFColor Nothing (Just 1) (Just 0)) (RTFColor (Just 0) (Just 0) (Just 0)) (CSSRGB 1 2 3)]
            , cfgTextMap = [TextMap "====" "****"]
            }
        )
        [multiline|
{
  "colorMap": [
    { "from": { "color": [null, 1, 0] }, "to":  { "color": [0, 0, 0], "colorSpace": { "cssrgb": [1,2,3] } } }
  ],
  "textMap": [
    { "pattern": "====", "replacement": "****" }
  ]
}
            |]

    it "should parse ColorMap" $ do
      let fromColor = RTFColor Nothing (Just 0) (Just 255)
          toColor = RTFColor (Just 1) (Just 2) (Just 3)
      testJSONParse (ColorMap fromColor toColor (CSSRGB 1 2 3)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "cssrgb": [1,2,3] } } } |]
      testJSONParse (ColorMap fromColor toColor (CSGenericRGB 1 2 3)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": [1,2,3] } } } |]
      testJSONParse (ColorMap fromColor toColor (CSGray 1)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgray": 1 } } } |]

rtfSpec :: Spec
rtfSpec = describe "RTF" $ do
  let

  describe "ToRTFDoc" $ do
    it "RTFHeader" $ do
      let s =
            [multiline|\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;}
              |]
      result <- expectToRTFDocSuccess @RTFHeader s
      result
        `shouldBe` RTFHeader
          { rtfCharset = Ansi 1252
          , rtfCocoaControls =
              [ CocoaControl "rtf" (Just 2639)
              , CocoaControl "textscaling" (Just 0)
              , CocoaControl "platform" (Just 0)
              ]
          , rtfFontTbl =
              FontTbl
                [ Just (FontInfo{fontNum = 0, fontFamily = FNil, fontCharset = Just 0, fontName = "HelveticaNeue"})
                , Just (FontInfo{fontNum = 1, fontFamily = FNil, fontCharset = Just 0, fontName = "Monaco"})
                ]
          , rtfColors =
              [ (RTFColor{red = Nothing, green = Nothing, blue = Nothing}, Nothing)
              , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Nothing)
              , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0))
              , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
              , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
              , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766))
              , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))
              ]
          }

    it "RTFDoc" $ do
      let s =
            [multiline|{\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;} a}
              |]
      result <- expectToRTFDocSuccess @RTFDoc s
      result
        `shouldBe` RTFDoc
          { rtfDocHeader =
              RTFHeader
                { rtfCharset = Ansi 1252
                , rtfCocoaControls =
                    [ CocoaControl "rtf" (Just 2639)
                    , CocoaControl "textscaling" (Just 0)
                    , CocoaControl "platform" (Just 0)
                    ]
                , rtfFontTbl =
                    FontTbl
                      [ Just (FontInfo{fontNum = 0, fontFamily = FNil, fontCharset = Just 0, fontName = "HelveticaNeue"})
                      , Just (FontInfo{fontNum = 1, fontFamily = FNil, fontCharset = Just 0, fontName = "Monaco"})
                      ]
                , rtfColors =
                    [ (RTFColor{red = Nothing, green = Nothing, blue = Nothing}, Nothing)
                    , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Nothing)
                    , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0))
                    , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
                    , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
                    , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766))
                    , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))
                    ]
                }
          , rtfDocContent = [RTFText " a"]
          }

  describe "process" $ do
    it "apply" $ do
      let config =
            ( Config
                { cfgColorMap = [ColorMap (RTFColor (Just 226) (Just 226) (Just 226)) (RTFColor (Just 230) (Just 230) (Just 230)) (CSSRGB 1 2 3)]
                , cfgTextMap = [TextMap "a" "b"]
                }
            )
      bytes <- T.readFile $ rtfPath </> "Default new.rtf"
      result <- expectToRTFDocSuccess @RTFDoc bytes
      applyConfig config result
        `shouldBe` ( RTFDoc
                      { rtfDocHeader =
                          RTFHeader
                            { rtfCharset = Ansi 1252
                            , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
                            , rtfFontTbl =
                                FontTbl
                                  [ Just
                                      FontInfo
                                        { fontNum = 0
                                        , fontFamily = FNil
                                        , fontCharset = Just 0
                                        , fontName = "HelveticaNeue"
                                        }
                                  ]
                            , rtfColors =
                                [
                                  ( RTFColor
                                      { red = Nothing
                                      , green = Nothing
                                      , blue = Nothing
                                      }
                                  , Nothing
                                  )
                                ,
                                  ( RTFColor
                                      { red = Just 255
                                      , green = Just 255
                                      , blue = Just 255
                                      }
                                  , Nothing
                                  )
                                ,
                                  ( RTFColor
                                      { red = Just 230
                                      , green = Just 230
                                      , blue = Just 230
                                      }
                                  , Just (CSSRGB 1 2 3)
                                  )
                                ]
                            }
                      , rtfDocContent =
                          [ RTFGroup
                              [ RTFControlWord NoPrefix "info" NoSuffix
                              , RTFGroup [RTFControlWord NoPrefix "author" SpaceSuffix, RTFText "Yui Nishizawa"]
                              ]
                          , RTFControlWord NoPrefix "pard" NoSuffix
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 566)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 1133)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 1700)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 2267)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 2834)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 3401)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 3968)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 4535)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 5102)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 5669)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 6236)
                          , RTFControlWord NoPrefix "tx" (RTFControlParam 6803)
                          , RTFControlWord NoPrefix "slleading" (RTFControlParam 24)
                          , RTFControlWord NoPrefix "pardirnatural" NoSuffix
                          , RTFControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
                          , RTFControlWord NoPrefix "f" (RTFControlParam 0)
                          , RTFControlWord NoPrefix "fs" (RTFControlParam 28)
                          , RTFText " "
                          , RTFControlWord NoPrefix "cf" (RTFControlParam 0)
                          , RTFText " \t\t\t\tb"
                          , RTFControlSymbol '\n'
                          , RTFControlSymbol '\n'
                          , RTFControlWord NoPrefix "cb" (RTFControlParam 2)
                          , RTFText " \t\tbbb"
                          ]
                      }
                   , [[2]]
                   , [1]
                   )

      views _1 render (applyConfig config result) `shouldBe` "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\fnil\\fcharset0 HelveticaNeue;}{\\colortbl;\\red255\\green255\\blue255;\\red230\\green230\\blue230;}{\\*\\expandedcolortbl;;\\cssrgb\\c1\\c2\\c3;}\n{\\info{\\author Yui Nishizawa}}\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\slleading24\\pardirnatural\\partightenfactor0\\f0\\fs28 \\cf0 \t\t\t\tb\\\n\\\n\\cb2 \t\tbbb}"

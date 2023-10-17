{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Spec (
  spec,
  configSpec,
) where

import Data.ByteString.Char8 qualified as B
import Data.Text.Encoding
import Notes.App
import Test.Hspec hiding (runIO)
import Test.Utils

spec :: Spec
spec = describe "main" $ do
  rtfSpec
  configSpec

configSpec :: SpecWith ()
configSpec =
  describe "Config" $ do
    let testJSONParse :: forall a. (Show a, Eq a, FromJSON a) => a -> Text -> Expectation
        testJSONParse expected text = do
          let result = eitherDecode' @a $ B.fromStrict $ encodeUtf8 text
          result `shouldBe` Right expected
        testJSONParseFail :: forall a. (Show a, Eq a, FromJSON a) => String -> Text -> Expectation
        testJSONParseFail expected text = do
          let result = eitherDecode' @a $ B.fromStrict $ encodeUtf8 text
          result `shouldBe` Left expected

    it "should parse Config" $ do
      testJSONParse
        ( Config
            { cfgColorMap = [ColorMap (RTFColor Nothing (Just 1) (Just 0)) (RTFColor (Just 0) (Just 0) (Just 0)) (CSSRGB 1 2 3 Nothing)]
            , cfgTextMap = [TextMap "====" "****"]
            , cfgContentMap = [ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| [])]
            , cfgFontMap = [FontMap "" (FontMapFont FNil "Papyrus")]
            }
        )
        [multiline|
{
  "colorMap": [
    { "from": { "color": [null, 1, 0] }, "to":  { "color": [0, 0, 0], "colorSpace": { "cssrgb": [1,2,3] } } }
  ],
  "textMap": [
    { "pattern": "====", "replacement": "****" }
  ],
  "contentMap": [
    { "fromContents": "\\'85", "toContents": "..." }
  ],
  "fontMap": [{ "fromFontName": "", "toFont": {"family": "nil", "fontName": "Papyrus"}}]
}
            |]

    it "should parse ColorMap" $ do
      let fromColor = RTFColor Nothing (Just 0) (Just 255)
          toColor = RTFColor (Just 1) (Just 2) (Just 3)
      testJSONParse (ColorMap fromColor toColor (CSSRGB 1 2 3 Nothing)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "cssrgb": [1,2,3] } } } |]
      testJSONParse (ColorMap fromColor toColor (CSGenericRGB 1 2 3 Nothing)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": [1,2,3] } } } |]
      testJSONParse (ColorMap fromColor toColor (CSGray 1)) [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgray": 1 } } } |]

    it "should parse FontFamily" $ do
      testJSONParse FNil "\"nil\""
      testJSONParse FRoman "\"roman\""
      testJSONParse FSwiss "\"swiss\""
      testJSONParse FModern "\"modern\""
      testJSONParse FScript "\"script\""
      testJSONParse FDecor "\"decor\""
      testJSONParse FTech "\"tech\""
      testJSONParse FBidi "\"bidi\""
      testJSONParseFail @FontFamily "Error in $: Unknown font family abc. Should be one of FNil,FRoman,FSwiss,FModern,FScript,FDecor,FTech,FBidi" "\"abc\""

    it "[error message] ColorMap" $ do
      testJSONParseFail @ColorMap "Error in $: key \"from\" not found" [multiline|{ "a": {}  } |]
      testJSONParseFail @ColorMap "Error in $.from: key \"color\" not found" [multiline|{ "from": {}  } |]
      testJSONParseFail @ColorMap "Error in $.from.color: Failed to parse array for RTFColor" [multiline|{ "from": { "color": [] }  } |]
      testJSONParseFail @ColorMap "Error in $.from.color[0]: parsing red failed, expected Number, but encountered String" [multiline|{ "from": { "color": ["a",1,2] }  } |]

      testJSONParseFail @ColorMap "Error in $: key \"to\" not found" [multiline|{ "from": { "color": [null,1,2] }  } |]
      testJSONParseFail @ColorMap "Error in $.to: key \"color\" not found" [multiline|{ "from": { "color": [null,1,2] }, "to": {} } |]
      testJSONParseFail @ColorMap "Error in $.to: key \"colorSpace\" not found" [multiline|{ "from": { "color": [null,1,2] }, "to": { "color": [1,2,3] } } |]

      testJSONParseFail @ColorMap "Error in $.to.colorSpace.csgray[0]: parsing CSGray failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgray": "a" } } } |]

      testJSONParseFail @ColorMap "Error in $.to.colorSpace.cssrgb[0]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "cssrgb": ["a",2,3] } } } |]
      testJSONParseFail @ColorMap "Error in $.to.colorSpace.cssrgb[1]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "cssrgb": [1,"a",2] } } } |]
      testJSONParseFail @ColorMap "Error in $.to.colorSpace.cssrgb[2]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "cssrgb": [1,2,"a"] } } } |]

      testJSONParseFail @ColorMap "Error in $.to.colorSpace.csgenericrgb[0]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": ["a",2,3] } } } |]
      testJSONParseFail @ColorMap "Error in $.to.colorSpace.csgenericrgb[1]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": [1,"a",3] } } } |]
      testJSONParseFail @ColorMap "Error in $.to.colorSpace.csgenericrgb[2]: parsing c failed, expected Number, but encountered String" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": [1,2,"a"] } } } |]
      testJSONParseFail @ColorMap "Error in $.to.colorSpace.csgenericrgb: Failed to parse array for CSGenericRGB" [multiline|{ "from": { "color": [null, 0, 255] }, "to":  { "color": [1, 2, 3], "colorSpace": { "csgenericrgb": [1,2,3,4] } } } |]

    it "[error message] FontMap" $ do
      testJSONParseFail @FontMap "Error in $: parsing Notes.Config.FontMap(FontMap) failed, key \"toFont\" not found" [multiline|{ "fromFontName": "Arial"  } |]
      testJSONParseFail @FontMap "Error in $.toFont: parsing Notes.Config.FontMapFont(FontMapFont) failed, key \"family\" not found" [multiline|{ "fromFontName": "Arial", "toFont" : {}  } |]
      testJSONParseFail @FontMap "Error in $.toFont: parsing Notes.Config.FontMapFont(FontMapFont) failed, unknown fields: [\"a\"]" [multiline|{ "fromFontName": "Arial", "toFont" : { "family": "roman", "fontName": "HelveticaNeue", "a": 0 }  } |]

    it "should parse ContentMap" $ do
      testJSONParse
        ( ContentMap
            { fromContents = ContentText "abc" :| []
            , toContents = ContentText "def" :| []
            }
        )
        [multiline|{ "fromContents": "abc", "toContents": "def" } |]

      testJSONParse
        ( ContentMap
            { fromContents =
                ContentEscapedSequence 130
                  :| [ ContentEscapedSequence 160
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 162
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 164
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 166
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 168
                     ]
            , toContents =
                ContentEscapedSequence 130
                  :| [ ContentEscapedSequence 169
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 171
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 173
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 175
                     , ContentEscapedSequence 130
                     , ContentEscapedSequence 177
                     ]
            }
        )
        -- あいうえお   -->     かきくけこ
        [multiline|{ "fromContents": "\\'82\\'a0\\'82\\'a2\\'82\\'a4\\'82\\'a6\\'82\\'a8", "toContents": "\\'82\\'a9\\'82\\'ab\\'82\\'ad\\'82\\'af\\'82\\'b1" } |]

    it "[error message] ContentMap" $ do
      testJSONParseFail @ContentMap "Error in $.fromContents: Empty" [multiline|{ "fromContents": "", "toContents": ""  } |]
      testJSONParseFail @ContentMap "Error in $.toContents: Empty" [multiline|{ "fromContents": "a", "toContents": ""  } |]
      testJSONParseFail @ContentMap "Error in $.fromContents: RTFElement:1:4:\n  |\n1 | {\\a\n  |    ^\nunexpected end of input\nexpecting '}', RTFControlParam, RTFElement, SpaceSuffix, name character, or newline\n" [multiline|{ "fromContents": "{\\a", "toContents": ""  } |]

rtfSpec :: Spec
rtfSpec = describe "ToRTFDoc" $ do
  it "ToRTFDoc @RTFHeader" $ do
    let text =
          [multiline|\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;}
              |]
    result <- expectToRTFDocSuccess @RTFHeader text
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
            , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0 Nothing))
            , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
            , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
            , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766 Nothing))
            , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000 Nothing))
            ]
        }

  it "ToRTFDoc @RTFDoc" $ do
    let text =
          [multiline|{\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;} a}
              |]
    result <- expectToRTFDocSuccess @RTFDoc text
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
                  , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0 Nothing))
                  , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
                  , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
                  , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766 Nothing))
                  , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000 Nothing))
                  ]
              }
        , rtfDocContent = [ContentText " a"]
        }

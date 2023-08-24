{-# LANGUAGE QuasiQuotes #-}

module Spec (
  spec,
) where

import Control.Monad
import Notes.Config

-- import Data.Attoparsec.ByteString hiding (parse)
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Language.Haskell.TH
import Notes.RTFDoc

-- import Notes.RTFDoc.RawParse
-- import Notes.RTFDoc.ToRTFDoc
import System.Directory
import System.FilePath
import Test.Hspec hiding (runIO)
import TestUtils
import Notes.Process

rtfFiles :: [FilePath]
rtfFiles =
  $( do
      let dir = rtfPath
      contents <-
        runIO
          ( getDirectoryContents dir
              <&> filter ((== ".rtf") . takeExtension)
              <&> ((dir </>) <$>)
          )
      [|contents|]
   )

spec :: Spec
spec = describe "main" $ do
  rtfSpec

  describe "Config" $ do
    let testJSONParse :: forall a. (Show a, Eq a, FromJSON a) => a -> ByteString -> Expectation
        testJSONParse expected text = do
          let result = eitherDecode' @a $ B.fromStrict text
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
    -- Note: newlines are insignificant in RTF.
    -- Hence, normalize the RTF text by removing new lines.
    -- However, the content text may have newlines, represented as an
    -- RTF symbol
    --      \\n
    -- To avoid replacing the newline RTF symbol, replace the symbol with a placeholder first.
    normalize :: Text -> Text
    normalize text =
      T.replace "\\\n" newlinePlaceholder text
        & T.replace "\n" ""
        -- Note: use placeholder for whitespace to highlight diff more clearly.
        & T.replace " " whitespacePlaholder
        & T.replace newlinePlaceholder "\\\n"
     where
      whitespacePlaholder = "@@@"
      newlinePlaceholder = "😄"

    -- tryParse :: Parser a -> ByteString -> IO a
    -- tryParse p d = do
    --   let result = parseOnly p d
    --   case result of
    --     Left msg -> do
    --       expectationFailure msg
    --       undefined
    --     Right v -> return v

    tryParse2 :: ToRTFDoc a => ByteString -> IO a
    tryParse2 d = do
      let result = parseDoc toRTFDoc d
      case result of
        Left msg -> do
          expectationFailure $ show msg
          fail $ show msg
        Right (v, _) -> return v

    testSampleRtf :: FilePath -> Spec
    testSampleRtf path = it ("sample: " <> path) $ do
      bytes <- B.readFile path
      -- result <- tryParse (parse @RTFDoc) bytes
      result <- tryParse2 @RTFDoc bytes
      let src = normalize (decodeUtf8 bytes)
          encoded = normalize (render result)
      when (encoded /= src) $ print result
      encoded `shouldBe` src
      T.length encoded `shouldBe` T.length src

  describe "parse" $ do
    it "Header" $ do
      let s =
            [multiline|\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;}
              |]
      result <- tryParse2 @RTFHeader s
      result
        `shouldBe` RTFHeader
          { rtfCharset = Ansi 1252
          , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
          , rtfFontTbl = FontTbl [Just (FontInfo{fontNum = 0, fontFamily = FNil, fontCharset = Just 0, fontName = "HelveticaNeue"}), Just (FontInfo{fontNum = 1, fontFamily = FNil, fontCharset = Just 0, fontName = "Monaco"})]
          , rtfColors = [(RTFColor{red = Nothing, green = Nothing, blue = Nothing}, Nothing), (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Nothing), (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0)), (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000)), (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525)), (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766)), (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))]
          }

    it "Doc" $ do
      let s =
            [multiline|{\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;} a}
              |]
      result <- tryParse2 @RTFDoc s
      result
        `shouldBe` RTFDoc
          { rtfDocHeader =
              RTFHeader
                { rtfCharset = Ansi 1252
                , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
                , rtfFontTbl = FontTbl [Just (FontInfo{fontNum = 0, fontFamily = FNil, fontCharset = Just 0, fontName = "HelveticaNeue"}), Just (FontInfo{fontNum = 1, fontFamily = FNil, fontCharset = Just 0, fontName = "Monaco"})]
                , rtfColors = [(RTFColor{red = Nothing, green = Nothing, blue = Nothing}, Nothing), (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Nothing), (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0)), (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000)), (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525)), (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766)), (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))]
                }
          , rtfDocContent = [RTFText " a"]
          }
    -- print result
    -- print rtfFiles

    foldr (\x r -> r >> testSampleRtf x) (pure ()) rtfFiles

  describe "" $ do
    it "apply" $ do
      let config = ( Config
              { cfgColorMap = [ColorMap (RTFColor (Just 226) (Just 226) (Just 226)) (RTFColor (Just 230) (Just 230) (Just 230)) (CSSRGB 1 2 3)]
              , cfgTextMap = [TextMap "a" "b"]
              }
            )
      bytes <- B.readFile $ rtfPath </> "Default new.rtf"
      result <- tryParse2 @RTFDoc bytes
      applyConfig config result `shouldBe` (result, [], [])

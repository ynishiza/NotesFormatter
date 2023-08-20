{-# LANGUAGE QuasiQuotes #-}

module Spec (
  spec,
) where

import Control.Monad
import Data.Attoparsec.ByteString hiding (parse)
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Language.Haskell.TH
import RTFDoc.RawParse
import RTFDoc
import System.Directory
import System.FilePath
import Test.Hspec hiding (runIO)
import TestUtils

rtfFiles :: [FilePath]
rtfFiles =
  $( do
      let dir = basePath </> "data"
      contents <-
        runIO
          ( getDirectoryContents dir
              <&> filter ((== ".rtf") . takeExtension)
              <&> ((dir </>) <$>)
          )
      [|contents|]
   )

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

tryParse :: Parser a -> ByteString -> IO a
tryParse p d = do
  let result = parseOnly p d
  case result of
    Left msg -> do
      expectationFailure msg
      undefined
    Right v -> return v

testSample :: FilePath -> Spec
testSample path = it ("sample: " <> path) $ do
  bytes <- B.readFile path
  result <- tryParse (parse @RTFDoc) bytes
  let src = normalize (decodeUtf8 bytes)
      encoded = normalize (render result)
  when (encoded /= src) $ print result
  encoded `shouldBe` src
  T.length encoded `shouldBe` T.length src

spec :: Spec
spec = describe "main" $ do
  let decodeDoc = parse @RTFDoc

  describe "sample parses" $ do
    it "Header" $ do
      let s =
            [multiline|\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fnil\fcharset0 Monaco;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red191\green191\blue191;
\red226\green226\blue226;\red0\green0\blue255;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\csgray\c79525;
\csgenericrgb\c88766\c88766\c88766;\cssrgb\c1680\c19835\c100000;}
              |]
      result <- tryParse (parse @RTFHeader) s
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
      result <- tryParse decodeDoc s
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

    foldr (\x r -> r >> testSample x) (pure ()) rtfFiles

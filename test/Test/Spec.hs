{-# LANGUAGE QuasiQuotes #-}

module Test.Spec (
  spec,
  processSpec,
) where

import Control.Lens
import Data.ByteString.Char8 qualified as B
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Notes.App
import Notes.RTFDoc
import Notes.RTFFile
import Shelly hiding (path, (</>))
import System.IO.Extra
import Test.Hspec hiding (runIO)
import Test.Utils

debugMode :: Bool
debugMode = False

logDebug :: String -> IO ()
logDebug str = when debugMode $ putStrLn str

testWorkspaceDir :: FilePath
testWorkspaceDir = basePath </> "test_workspace"

-- Note: file paths are absolute so the result format by default depends on the file system being run.
-- Make the paths relative for testability.
formatResultForTest :: FilePath -> [(RTFFile filetype, FilePath, ProcessResult)] -> [(RTFFile filetype, FilePath, ProcessResult)]
formatResultForTest base result =
  result
    & over (traverse . _1) (makeRTFFilePathRelativeTo base)
    & over (traverse . _2) (makePathRelativeTo base)

spec :: Spec
spec = describe "main" $ do
  rtfSpec
  configSpec
  processSpec

configSpec :: SpecWith ()
configSpec =
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
            , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0))
            , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
            , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
            , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766))
            , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))
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
                  , (RTFColor{red = Just 0, green = Just 0, blue = Just 0}, Just (CSSRGB 0 0 0))
                  , (RTFColor{red = Just 255, green = Just 255, blue = Just 255}, Just (CSGray 100000))
                  , (RTFColor{red = Just 191, green = Just 191, blue = Just 191}, Just (CSGray 79525))
                  , (RTFColor{red = Just 226, green = Just 226, blue = Just 226}, Just (CSGenericRGB 88766 88766 88766))
                  , (RTFColor{red = Just 0, green = Just 0, blue = Just 255}, Just (CSSRGB 1680 19835 100000))
                  ]
              }
        , rtfDocContent = [RTFText " a"]
        }

processSpec :: SpecWith ()
processSpec =
  describe "Process" $ do
    it "[applyConfig]" $ do
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
                   , ProcessResult
                      [ (config ^?! (_cfgColorMap . element 0), [2])
                      ]
                      [ (config ^?! (_cfgTextMap . element 0), 1)
                      ]
                   )

      views _1 render (applyConfig config result) `shouldBe` "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\fnil\\fcharset0 HelveticaNeue;}{\\colortbl;\\red255\\green255\\blue255;\\red230\\green230\\blue230;}{\\*\\expandedcolortbl;;\\cssrgb\\c1\\c2\\c3;}\n{\\info{\\author Yui Nishizawa}}\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\slleading24\\pardirnatural\\partightenfactor0\\f0\\fs28 \\cf0 \t\t\t\tb\\\n\\\n\\cb2 \t\tbbb}"

    let
      testConfig =
        Config
          { cfgColorMap =
              [ ColorMap
                  { fromColor = RTFColor (Just 226) (Just 226) (Just 226)
                  , toColor = RTFColor (Just 107) (Just 0) (Just 108)
                  , toColorSpace = CSGenericRGB 41819 0 42431
                  }
              ]
          , cfgTextMap =
              [ TextMap "==============================================================" "*****************************************************************************"
              ]
          }

    beforeAll_
      -- Setup: clear test space
      ( shelly $ rm_rf testWorkspaceDir >> mkdir_p testWorkspaceDir
      )
      $ describe "process files"
      $ do
        it "[processDirRTF]" $ do
          let dataSrcDir = dataPath </> "database" </> "rtf"
              testSpaceDir = testWorkspaceDir </> "processDirRTF"
              testSpaceDataDir = testSpaceDir </> "data"
              testSpaceBackupDir = testSpaceDir </> "bak"
              logPath = testSpaceDir </> "log.txt"

          appOptions <- mkAppOtions testSpaceBackupDir testConfig
          let timestamp = formatTimestamp (appTime appOptions)

          shelly $ do
            -- Setup: create test space
            rm_rf testSpaceDir
            mkdir_p testSpaceDir
            -- Setup: copy test data to test space
            cp_r dataSrcDir testSpaceDataDir

          (io, result) <- captureOutput $ runApp logPath appOptions (processDirRTF testSpaceDataDir)
          logDebug io

          formatResultForTest basePath result
            `shouldBe` over
              (traverse . _2)
              (prependToFileName timestamp)
              [
                ( RTFFile "/test_workspace/processDirRTF/data/usage_cocoa.rtf"
                , "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_usage_cocoa.rtf"
                , ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [3, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]}
                )
              , (RTFFile "/test_workspace/processDirRTF/data/Modules- testing - Hedgehog.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_Modules- testing - Hedgehog.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/Info.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_Info.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 3)]})
              , (RTFFile "/test_workspace/processDirRTF/data/TestCases.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_TestCases.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/[Network]- LDAP (Lightweight Directory Access Protocol).rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- LDAP (Lightweight Directory Access Protocol).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- neo4j functions, aggregates.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- neo4j functions, aggregates.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- React Hooks.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- React Hooks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Command [Linux]- resolvconf.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [Linux]- resolvconf.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- JavaScript lib - immutable.js.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- JavaScript lib - immutable.js.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/[Network]- mDNS (Multicast DNS).rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- mDNS (Multicast DNS).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/[Programming]- Language theory - Grammar.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Programming]- Language theory - Grammar.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Command- dig.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- dig.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Command- ip.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- ip.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- Lua tricks.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Lua tricks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- Docker registry, repository, images.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Docker registry, repository, images.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Plugin- Taglist.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Plugin- Taglist.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Tool- Stack.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Tool- Stack.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 3)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Command [macOS] tricks.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [macOS] tricks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Notes- Windows 10.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Notes- Windows 10.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/20/Cheatsheet- Berkley Packet Filter (BPF).rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Berkley Packet Filter (BPF).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Task- To do.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Task- To do.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Cheatsheet- Vimperator.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Vimperator.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Cheatsheet- Node.js lib Express.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Node.js lib Express.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Command [Linux]- sysctl.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Command [Linux]- sysctl.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Cheatsheet- New Relic.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- New Relic.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Modules- testing - Hedgehog.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- testing - Hedgehog.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Reading- 中原中也.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Reading- \20013\21407\20013\20063.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/Modules- monad - mtl.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- monad - mtl.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/18/[HTTP]- HTTP Cache.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_[HTTP]- HTTP Cache.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Cheatsheet- Node.js libs.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Node.js libs.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 13)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Cheatsheet- iOS - usage.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- iOS - usage.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Svetlana Alexievich- Chernobyl prayer.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Svetlana Alexievich- Chernobyl prayer.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Modules- monad - transformers.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- monad - transformers.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Modules- megaparsec.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- megaparsec.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 3)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Cheatsheet- C CLI - gcc-nm-objdump.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- C CLI - gcc-nm-objdump.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Cheatsheet- Homebrew.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Homebrew.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/[Network]- IPv4 CIDR and class network.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- IPv4 CIDR and class network.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [7])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/[Network]- Cheatsheet.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- Cheatsheet.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Command- mount.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- mount.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Command- w.rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- w.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFFile "/test_workspace/processDirRTF/data/2a/Cheatsheet- bash - variables (EXPANSION).rtf", "/test_workspace/processDirRTF/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- bash - variables (EXPANSION).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              ]

        it "[processDirRTFD]" $ do
          let dataSrcDir = dataPath </> "database" </> "rtfd"
              testSpaceDir = testWorkspaceDir </> "processDirRTFD"
              testSpaceDataDir = testSpaceDir </> "data"
              testSpaceBackupDir = testSpaceDir </> "bak"
              logPath = testSpaceDir </> "log.txt"

          appOptions <- mkAppOtions testSpaceBackupDir testConfig
          let timestamp = formatTimestamp (appTime appOptions)

          shelly $ do
            -- Setup: create test space
            rm_rf testSpaceDir
            mkdir_p testSpaceDir
            -- Setup: copy test data to test space
            cp_r dataSrcDir testSpaceDataDir

          (io, result) <- captureOutput $ runApp logPath appOptions (processDirRTFD testSpaceDataDir)
          logDebug io
          formatResultForTest basePath result
            `shouldBe` over
              (traverse . _2)
              (prependToFileName timestamp)
              [ (RTFDFile "/test_workspace/processDirRTFD/data/1f/Cheatsheet- snort v3 pulledpork.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Cheatsheet- snort v3 pulledpork.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [3])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/1f/Concept- Numbers - Floating point.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Concept- Numbers - Floating point.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/1a/[Network]- Network Address Translation (NAT).rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_[Network]- Network Address Translation (NAT).rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/1a/Notes- syslog.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_Notes- syslog.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/c/Tool- Stack usage.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Tool- Stack usage.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/c/Cheatsheet- tricks - editing.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Cheatsheet- tricks - editing.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/c/[Distributed systems]- CAP theorem.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_[Distributed systems]- CAP theorem.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/4/[Security]- general.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_[Security]- general.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/4/Cheatsheet- neovim LSP usage + troubleshooting.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- neovim LSP usage + troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/4/Cheatsheet- TeX-LaTeX tricks.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- TeX-LaTeX tricks.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/4/Notes- Redis - Redlock.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Notes- Redis - Redlock.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/4/Cheatsheet- basics - Template Haskell.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- basics - Template Haskell.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 5)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/Cheatsheet- gpg usage-1.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_Cheatsheet- gpg usage-1.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 4)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/e/Notes- SQL.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Notes- SQL.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 5)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/e/Food- Olive oil - Balasmic Vinegar.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Food- Olive oil - Balasmic Vinegar.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/e/Command- ssh tricks + troubleshooting.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Command- ssh tricks + troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              , (RTFDFile "/test_workspace/processDirRTFD/data/e/Cheatsheet- macOS usage - troubleshooting.rtfd", "/test_workspace/processDirRTFD/bak/Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Cheatsheet- macOS usage - troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)]})
              ]

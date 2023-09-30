module Test.SpecApp (
  spec,
) where

import Control.Lens
import Data.List (intercalate, sortOn)
import Data.List.Extra (replace)
import Data.Text.IO qualified as T
import Notes.App
import Notes.RTFDoc
import Notes.RTFFile
import Shelly hiding (path, (</>))
import System.FilePath
import System.IO.Extra
import Test.Hspec hiding (runIO)
import Test.Utils

debugMode :: Bool
debugMode = False

logDebug :: String -> IO ()
logDebug str = when debugMode $ putStrLn str

testWorkspaceDir :: FilePath
testWorkspaceDir = basePath </> "test_workspace"

-- Note: the result contains data that may be system/run dependent

-- * paths are absolute: need to make relative

-- * backup file name contains a timestamp
formatResultForTest :: String -> FilePath -> FilePath -> [(RTFFile filetype, FilePath, ProcessResult)] -> [(RTFFile filetype, FilePath, ProcessResult)]
formatResultForTest timestamp oldBase newBase result =
  result
    & over (traverse . _1) (changeRTFFilePathBase oldBase newBase)
    & over (traverse . _2) (changePathBase oldBase newBase)
    & over (traverse . _2) replaceTimestamp
    & over (traverse . _2) (replace (backupName oldBase) (backupName newBase))
    & sortOn (views _1 rtfFilePath)
 where
  replaceTimestamp path = takeDirectory path </> newName
   where
    newName =
      takeFileName path
        & drop (length timestamp)
        & (timestamp <>)
  backupName base =
    base
      & splitDirectories
      & filter (/= "/")
      & intercalate "_"

spec :: SpecWith ()
spec =
  describe "Process" $ do
    it "[applyConfig]" $ do
      let config =
            ( Config
                { cfgColorMap = [ColorMap (RTFColor (Just 226) (Just 226) (Just 226)) (RTFColor (Just 230) (Just 230) (Just 230)) (CSSRGB 1 2 3)]
                , cfgTextMap = [TextMap "a" "b"]
                , cfgFontMap = [FontMap "HelveticaNeue" (FontMapFont FRoman "TimesNewRomanPSMT")]
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
                                        , fontFamily = FRoman
                                        , fontCharset = Just 0
                                        , fontName = "TimesNewRomanPSMT"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 1
                                        , fontFamily = FNil
                                        , fontCharset = Just 0
                                        , fontName = "Monaco"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 2
                                        , fontFamily = FNil
                                        , fontCharset = Just 0
                                        , fontName = "HelveticaNeue-Bold"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 3
                                        , fontFamily = FNil
                                        , fontCharset = Just 128
                                        , fontName = "HiraginoSans-W3"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 4
                                        , fontFamily = FSwiss
                                        , fontCharset = Just 0
                                        , fontName = "ArialMT"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 5
                                        , fontFamily = FNil
                                        , fontCharset = Just 0
                                        , fontName = "ComicSansMS"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 6
                                        , fontFamily = FModern
                                        , fontCharset = Just 0
                                        , fontName = "CourierNewPSMT"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 7
                                        , fontFamily = FSwiss
                                        , fontCharset = Just 0
                                        , fontName = "Helvetica"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 8
                                        , fontFamily = FRoman
                                        , fontCharset = Just 0
                                        , fontName = "Times-Roman"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 9
                                        , fontFamily = FRoman
                                        , fontCharset = Just 0
                                        , fontName = "TimesNewRomanPSMT"
                                        }
                                  , Just
                                      FontInfo
                                        { fontNum = 10
                                        , fontFamily = FNil
                                        , fontCharset = Just 0
                                        , fontName = "Verdana"
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
                                      { red = Just 0
                                      , green = Just 0
                                      , blue = Just 0
                                      }
                                  , Just (CSSRGB 0 0 0)
                                  )
                                ,
                                  ( RTFColor
                                      { red = Just 255
                                      , green = Just 255
                                      , blue = Just 255
                                      }
                                  , Just (CSGray 100000)
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
                      , rtfDocContent = [RTFGroup [RTFControlWord NoPrefix "info" NoSuffix, RTFGroup [RTFControlWord NoPrefix "author" SpaceSuffix, RTFText "Yui Nishizawa"]], RTFControlWord NoPrefix "pard" NoSuffix, RTFControlWord NoPrefix "pardeftab" (RTFControlParam 720), RTFControlWord NoPrefix "partightenfactor" (RTFControlParam 0), RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFControlWord NoPrefix "fs" (RTFControlParam 28), RTFText " ", RTFControlWord NoPrefix "cf" (RTFControlParam 2), RTFText " ", RTFControlWord NoPrefix "cb" (RTFControlParam 3), RTFText " ", RTFControlWord NoPrefix "expnd" (RTFControlParam 0), RTFControlWord NoPrefix "expndtw" (RTFControlParam 0), RTFControlWord NoPrefix "kerning" (RTFControlParam 0), RTFText "This is b test", RTFControlSymbol '\n', RTFControlSymbol '\n', RTFText "Normbl text", RTFControlSymbol '\n', RTFControlSymbol '\n', RTFControlWord NoPrefix "pard" NoSuffix, RTFControlWord NoPrefix "tx" (RTFControlParam 566), RTFControlWord NoPrefix "tx" (RTFControlParam 1133), RTFControlWord NoPrefix "tx" (RTFControlParam 1700), RTFControlWord NoPrefix "tx" (RTFControlParam 2267), RTFControlWord NoPrefix "tx" (RTFControlParam 2834), RTFControlWord NoPrefix "tx" (RTFControlParam 3401), RTFControlWord NoPrefix "tx" (RTFControlParam 3968), RTFControlWord NoPrefix "tx" (RTFControlParam 4535), RTFControlWord NoPrefix "tx" (RTFControlParam 5102), RTFControlWord NoPrefix "tx" (RTFControlParam 5669), RTFControlWord NoPrefix "tx" (RTFControlParam 6236), RTFControlWord NoPrefix "tx" (RTFControlParam 6803), RTFControlWord NoPrefix "slleading" (RTFControlParam 24), RTFControlWord NoPrefix "pardirnatural" NoSuffix, RTFControlWord NoPrefix "partightenfactor" (RTFControlParam 0), RTFControlWord NoPrefix "f" (RTFControlParam 1), RTFText " ", RTFControlWord NoPrefix "cf" (RTFControlParam 2), RTFText " ", RTFControlWord NoPrefix "cb" (RTFControlParam 4), RTFText " ", RTFControlSymbol '\n', RTFText "Code block", RTFControlSymbol '\n', RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " ", RTFControlWord NoPrefix "cf" (RTFControlParam 0), RTFText " ", RTFControlWord NoPrefix "cb" (RTFControlParam 1), RTFText " ", RTFControlWord NoPrefix "kerning" (RTFControlParam 1), RTFControlWord NoPrefix "expnd" (RTFControlParam 0), RTFControlWord NoPrefix "expndtw" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 2), RTFControlWord NoPrefix "b" SpaceSuffix, RTFText "Bold text\n", RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFControlWord NoPrefix "b" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlWord NoPrefix "ul" SpaceSuffix, RTFText "Underline text", RTFControlSymbol '\n', RTFControlWord NoPrefix "ulnone" SpaceSuffix, RTFControlWord NoPrefix "strike" SpaceSuffix, RTFControlWord NoPrefix "strikec" (RTFControlParam 0), RTFText " Strikethrough", RTFControlWord NoPrefix "ulnone" SpaceSuffix, RTFControlWord NoPrefix "strike" (RTFControlParam 0), RTFControlWord NoPrefix "striked" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 3), RTFText " ", RTFControlSymbol '\n', RTFControlSymbol '\'', RTFText "93", RTFControlSymbol '\'', RTFText "fb", RTFControlSymbol '\'', RTFText "96", RTFControlSymbol '\'', RTFText "7b", RTFControlSymbol '\'', RTFText "8c", RTFControlSymbol '\'', RTFText "eb\n", RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlSymbol '\n', RTFControlSymbol '\n', RTFControlWord NoPrefix "ul" SpaceSuffix, RTFText "Fonts", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 4), RTFControlWord NoPrefix "fs" (RTFControlParam 48), RTFText " ", RTFControlWord NoPrefix "ulnone" SpaceSuffix, RTFText "Aribl ", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 5), RTFText " Comic sbns\n", RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 6), RTFText " Courier New", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 7), RTFText " Helveticb", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " Helveticb Neue", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 8), RTFText " Times\n", RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 9), RTFText " Times New Rombn", RTFControlSymbol '\n', RTFControlWord NoPrefix "f" (RTFControlParam 10), RTFText " Verdbnb\n", RTFControlWord NoPrefix "f" (RTFControlParam 0), RTFText " ", RTFControlSymbol '\n', RTFControlSymbol '\n', RTFControlSymbol '\n', RTFText "Andble Mono", RTFControlSymbol '\n', RTFText "Monbco"]
                      }
                   , ProcessResult
                      { resultMapColor =
                          [
                            ( ColorMap
                                { fromColor =
                                    RTFColor
                                      { red = Just 226
                                      , green = Just 226
                                      , blue = Just 226
                                      }
                                , toColor =
                                    RTFColor
                                      { red = Just 230
                                      , green = Just 230
                                      , blue = Just 230
                                      }
                                , toColorSpace = CSSRGB 1 2 3
                                }
                            , [4]
                            )
                          ]
                      , resultMapText =
                          [
                            ( TextMap
                                { pattern = "a"
                                , replacement = "b"
                                }
                            , 12
                            )
                          ]
                      , resultMapFont =
                          [
                            ( FontMap
                                { fromFontName = "HelveticaNeue"
                                , toFont =
                                    FontMapFont
                                      { fmFamily = FRoman
                                      , fmFontName = "TimesNewRomanPSMT"
                                      }
                                }
                            , [0]
                            )
                          ]
                      }
                   )

      views _1 render (applyConfig config result)
        `shouldBe` "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\froman\\fcharset0 TimesNewRomanPSMT;\\f1\\fnil\\fcharset0 Monaco;\\f2\\fnil\\fcharset0 HelveticaNeue-Bold;\\f3\\fnil\\fcharset128 HiraginoSans-W3;\\f4\\fswiss\\fcharset0 ArialMT;\\f5\\fnil\\fcharset0 ComicSansMS;\\f6\\fmodern\\fcharset0 CourierNewPSMT;\\f7\\fswiss\\fcharset0 Helvetica;\\f8\\froman\\fcharset0 Times-Roman;\\f9\\froman\\fcharset0 TimesNewRomanPSMT;\\f10\\fnil\\fcharset0 Verdana;}{\\colortbl;\\red255\\green255\\blue255;\\red0\\green0\\blue0;\\red255\\green255\\blue255;\\red230\\green230\\blue230;}{\\*\\expandedcolortbl;;\\cssrgb\\c0\\c0\\c0;\\csgray\\c100000;\\cssrgb\\c1\\c2\\c3;}\n{\\info{\\author Yui Nishizawa}}\\pard\\pardeftab720\\partightenfactor0\\f0\\fs28 \\cf2 \\cb3 \\expnd0\\expndtw0\\kerning0This is b test\\\n\\\nNormbl text\\\n\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\slleading24\\pardirnatural\\partightenfactor0\\f1 \\cf2 \\cb4 \\\nCode block\\\n\\\n\\f0 \\cf0 \\cb1 \\kerning1\\expnd0\\expndtw0 \\\n\\f2\\b Bold text\n\\f0\\b0 \\\n\\ul Underline text\\\n\\ulnone \\strike \\strikec0 Strikethrough\\ulnone \\strike0\\striked0 \\\n\\f3 \\\n\\'93\\'fb\\'96\\'7b\\'8c\\'eb\n\\f0 \\\n\\\n\\\n\\ul Fonts\\\n\\f4\\fs48 \\ulnone Aribl \\\n\\f5 Comic sbns\n\\f0 \\\n\\f6 Courier New\\\n\\f7 Helveticb\\\n\\f0 Helveticb Neue\\\n\\f8 Times\n\\f0 \\\n\\f9 Times New Rombn\\\n\\f10 Verdbnb\n\\f0 \\\n\\\n\\\nAndble Mono\\\nMonbco}"

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
          , cfgFontMap = [FontMap "HelveticaNeue" (FontMapFont FRoman "TimesNewRomanPSMT")]
          }

    beforeAll_
      -- Setup: clear test space
      ( shelly $ rm_rf testWorkspaceDir >> mkdir_p testWorkspaceDir
      )
      $ describe "files"
      $ do
        it "[processDirRTF]" $ do
          let dataSrcDir = dataPath </> "database" </> "rtf"
              testSpaceDir = testWorkspaceDir </> "processDirRTF"
              testSpaceDataDir = testSpaceDir </> "data"
              testSpaceBackupDir = testSpaceDir </> "bak"
              logPath = testSpaceDir </> "log.txt"

          appOptions <-
            emptyAppOptions
              <&> \opts -> opts{appLogLevel = LevelInfo, appBackupDir = testSpaceBackupDir, appConfig = testConfig}
          let timestamp = formatTimestamp (appTime appOptions)

          shelly $ do
            -- Setup: create test space
            rm_rf testSpaceDir
            mkdir_p testSpaceDir
            -- Setup: copy test data to test space
            cp_r dataSrcDir testSpaceDataDir

          (io, result) <- captureOutput $ runApp logPath appOptions (processDirRTF testSpaceDataDir)
          logDebug io

          formatResultForTest timestamp basePath basePath result
            `shouldBe` formatResultForTest
              timestamp
              "/Users/yuinishizawa/Projects/NotesFormatter"
              basePath
              [ (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Cheatsheet- New Relic.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- New Relic.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Cheatsheet- Node.js lib Express.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Node.js lib Express.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Cheatsheet- Vimperator.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Vimperator.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Command [Linux]- sysctl.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Command [Linux]- sysctl.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Modules- monad - mtl.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- monad - mtl.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Modules- testing - Hedgehog.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- testing - Hedgehog.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Reading- 中原中也.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Reading- \20013\21407\20013\20063.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Task- To do.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Task- To do.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/[HTTP]- HTTP Cache.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_[HTTP]- HTTP Cache.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Berkley Packet Filter (BPF).rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Berkley Packet Filter (BPF).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Docker registry, repository, images.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Docker registry, repository, images.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- JavaScript lib - immutable.js.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- JavaScript lib - immutable.js.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Lua tricks.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Lua tricks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- React Hooks.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- React Hooks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- neo4j functions, aggregates.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- neo4j functions, aggregates.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command [Linux]- resolvconf.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [Linux]- resolvconf.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command [macOS] tricks.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [macOS] tricks.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command- dig.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- dig.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command- ip.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- ip.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Notes- Windows 10.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Notes- Windows 10.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Plugin- Taglist.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Plugin- Taglist.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Tool- Stack.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Tool- Stack.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 3)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Network]- LDAP (Lightweight Directory Access Protocol).rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- LDAP (Lightweight Directory Access Protocol).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Network]- mDNS (Multicast DNS).rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- mDNS (Multicast DNS).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [1])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Programming]- Language theory - Grammar.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Programming]- Language theory - Grammar.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [1])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- C CLI - gcc-nm-objdump.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- C CLI - gcc-nm-objdump.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- Homebrew.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Homebrew.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- Node.js libs.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Node.js libs.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 13)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- bash - variables (EXPANSION).rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- bash - variables (EXPANSION).rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- iOS - usage.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- iOS - usage.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Command- mount.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- mount.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [1])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Command- w.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- w.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [1])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Modules- megaparsec.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- megaparsec.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 3)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Modules- monad - transformers.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- monad - transformers.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Svetlana Alexievich- Chernobyl prayer.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Svetlana Alexievich- Chernobyl prayer.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/[Network]- Cheatsheet.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- Cheatsheet.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/[Network]- IPv4 CIDR and class network.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- IPv4 CIDR and class network.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [7])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/Modules- testing - Hedgehog.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_Modules- testing - Hedgehog.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/TestCases.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_TestCases.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/usage_cocoa.rtf", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10012023_0635_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_usage_cocoa.rtf", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [3, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              ]

        it "[processDirRTFD]" $ do
          let dataSrcDir = dataPath </> "database" </> "rtfd"
              testSpaceDir = testWorkspaceDir </> "processDirRTFD"
              testSpaceDataDir = testSpaceDir </> "data"
              testSpaceBackupDir = testSpaceDir </> "bak"
              logPath = testSpaceDir </> "log.txt"

          appOptions <-
            emptyAppOptions
              <&> \opts -> opts{appLogLevel = LevelInfo, appBackupDir = testSpaceBackupDir, appConfig = testConfig}
          let timestamp = formatTimestamp (appTime appOptions)

          shelly $ do
            -- Setup: create test space
            rm_rf testSpaceDir
            mkdir_p testSpaceDir
            -- Setup: copy test data to test space
            cp_r dataSrcDir testSpaceDataDir

          (io, result) <- captureOutput $ runApp logPath appOptions (processDirRTFD testSpaceDataDir)
          logDebug io
          formatResultForTest timestamp basePath basePath result
            `shouldBe` formatResultForTest
              timestamp
              "/Users/yuinishizawa/Projects/NotesFormatter"
              basePath
              [ (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1a/Notes- syslog.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_Notes- syslog.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1a/[Network]- Network Address Translation (NAT).rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_[Network]- Network Address Translation (NAT).rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1f/Cheatsheet- snort v3 pulledpork.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Cheatsheet- snort v3 pulledpork.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [3])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1f/Concept- Numbers - Floating point.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Concept- Numbers - Floating point.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- TeX-LaTeX tricks.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- TeX-LaTeX tricks.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- basics - Template Haskell.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- basics - Template Haskell.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5, 6])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 5)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- neovim LSP usage + troubleshooting.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- neovim LSP usage + troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 2)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Notes- Redis - Redlock.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Notes- Redis - Redlock.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/[Security]- general.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_[Security]- general.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/Cheatsheet- gpg usage-1.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_Cheatsheet- gpg usage-1.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [5])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 4)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/Cheatsheet- tricks - editing.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Cheatsheet- tricks - editing.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/Tool- Stack usage.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Tool- Stack usage.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/[Distributed systems]- CAP theorem.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_[Distributed systems]- CAP theorem.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 0)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [1])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Cheatsheet- macOS usage - troubleshooting.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Cheatsheet- macOS usage - troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Command- ssh tricks + troubleshooting.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Command- ssh tricks + troubleshooting.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [4])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Food- Olive oil - Balasmic Vinegar.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Food- Olive oil - Balasmic Vinegar.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 1)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              , (RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Notes- SQL.rtfd", "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Notes- SQL.rtfd", ProcessResult{resultMapColor = [(ColorMap{fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}, toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}, toColorSpace = CSGenericRGB 41819 0 42431}, [])], resultMapText = [(TextMap{pattern = "==============================================================", replacement = "*****************************************************************************"}, 5)], resultMapFont = [(FontMap{fromFontName = "HelveticaNeue", toFont = FontMapFont{fmFamily = FRoman, fmFontName = "TimesNewRomanPSMT"}}, [0])]})
              ]

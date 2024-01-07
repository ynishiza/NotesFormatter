module Test.SpecApp (
  spec,
) where

import Control.Lens hiding ((<.>))
import Data.List (intercalate, sortOn)
import Data.List.Extra (replace)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Notes.App
import Notes.RTFDoc
import Notes.RTFFile
import Shelly hiding (path, (<.>), (</>))
import System.FilePath
import System.IO.Extra
import Test.Data.ColorRTF qualified as ColorRTF
import Test.Data.FontRTF qualified as FontRTF
import Test.Data.SpecialCharactersRTF qualified as SpecialCharactersRTF
import Test.Data.StyleRTF qualified as StyleRTF
import Test.Data.TableRTF qualified as TableRTF
import Test.Hspec hiding (runIO)
import Test.Utils

debugMode :: Bool
debugMode = False

logDebug :: String -> IO ()
logDebug str = when debugMode $ putStrLn str

testWorkspaceDir :: FilePath
testWorkspaceDir = basePath </> "test_workspace"

{-
Note: the result contains data that may be system/run dependent
\* paths are absolute: need to make relative
\* backup file name contains a timestamp
-}
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
    let config =
          ( Config
              { cfgColorMap = [ColorMap (RTFColor (Just 226) (Just 226) (Just 226)) (RTFColor (Just 230) (Just 230) (Just 230)) (CSSRGB 1 2 3 Nothing)]
              , cfgTextMap = [TextMap "a" "b"]
              , cfgContentMap =
                  [ ContentMap 128 (contentEscapedSequence' "82" :| [contentEscapedSequence' "a0"]) (ContentText "AAA" :| [])
                  , ContentMap 0 (contentEscapedSequence' "85" :| [contentEscapedSequence' "a5"]) (ContentText "...Yen" :| [])
                  ]
              , cfgFontMap = [FontMap "HelveticaNeue" (FontMapFont FRoman (Just 0) "TimesNewRomanPSMT")]
              }
          )
    let
      testApplyConfig :: (HasCallStack) => String -> RTFDoc -> ProcessResult -> Text -> Spec
      testApplyConfig name expectedDoc expectedResult expectedText =
        it ("[applyConfig] " <> name) $ do
          bytes <- T.readFile $ rtfPath </> name
          parsed <- expectToRTFDocSuccess @RTFDoc bytes
          (doc, result) <- applyConfig config parsed
          let renderName = "_" <> dropExtension name <.> "rtf"
          T.writeFile renderName $ render doc

          rtfDocHeader doc `shouldBe` rtfDocHeader expectedDoc
          rtfDocContent doc `shouldBe` rtfDocContent expectedDoc
          result `shouldBe` expectedResult
          T.strip (render doc) `shouldBe` T.strip expectedText

    testApplyConfig
      "Font.rtf"
      FontRTF.content
      ( ProcessResult
          { resultMapColor = zip (cfgColorMap config) [[]]
          , resultMapText = zip (cfgTextMap config) [8]
          , resultMapFont = zip (cfgFontMap config) [[0]]
          , resultMapContent = zip (cfgContentMap config) [0, 0]
          }
      )
      FontRTF.contentText

    testApplyConfig
      "Color.rtf"
      ColorRTF.content
      ( ProcessResult
          { resultMapColor = zip (cfgColorMap config) [[4]]
          , resultMapText = zip (cfgTextMap config) [1]
          , resultMapFont = zip (cfgFontMap config) [[0]]
          , resultMapContent = zip (cfgContentMap config) [0, 0]
          }
      )
      ColorRTF.contentText

    testApplyConfig
      "Styles.rtf"
      StyleRTF.content
      ( ProcessResult
          { resultMapColor = zip (cfgColorMap config) [[4]]
          , resultMapText = zip (cfgTextMap config) [4]
          , resultMapFont = zip (cfgFontMap config) [[0]]
          , resultMapContent = zip (cfgContentMap config) [0, 0]
          }
      )
      StyleRTF.contentText

    testApplyConfig
      "SpecialCharacters.rtf"
      SpecialCharactersRTF.content
      ( ProcessResult
          { resultMapColor = zip (cfgColorMap config) [[]]
          , resultMapText = zip (cfgTextMap config) [5]
          , resultMapFont = zip (cfgFontMap config) [[1]]
          , resultMapContent = zip (cfgContentMap config) [1, 1]
          }
      )
      SpecialCharactersRTF.contentText

    testApplyConfig
      "Table.rtf"
      TableRTF.content
      ( ProcessResult
          { resultMapColor = zip (cfgColorMap config) [[]]
          , resultMapText = zip (cfgTextMap config) [3]
          , resultMapFont = zip (cfgFontMap config) [[1]]
          , resultMapContent = zip (cfgContentMap config) [0, 0]
          }
      )
      TableRTF.contentText

    it "[error] throws an error if the TextMap pattern is empty" $ do
      bytes <- T.readFile $ rtfPath </> "Font.rtf"
      parsed <- expectToRTFDocSuccess @RTFDoc bytes
      applyConfig (config{cfgTextMap = [TextMap "" ""]}) parsed
        `shouldThrow` ( \case
                          (TextMapError msg) -> msg == "Text map pattern cannot be empty"
                          _ -> False
                      )

    it "[error] throws an error if the FontMap charsets do not match" $ do
      let badFontMap =
            FontMap
              { fromFontName = "HelveticaNeue"
              , toFont =
                  FontMapFont
                    { fmFamily = FRoman
                    , -- BAD. Should be 0
                      fmCharset = Just 1
                    , fmFontName = "TimesNewRomanPSMT"
                    }
              }
      bytes <- T.readFile $ rtfPath </> "Font.rtf"
      parsed <- expectToRTFDocSuccess @RTFDoc bytes
      applyConfig (config{cfgFontMap = [badFontMap]}) parsed
        `shouldThrow` ( \case
                          (FontMapError msg) -> msg == "Charset mismatch mapping FontInfo {fontNum = 0, fontFamily = FNil, fontCharset = Just 0, fontName = \"HelveticaNeue\"} to FontMapFont {fmFamily = FRoman, fmCharset = Just 1, fmFontName = \"TimesNewRomanPSMT\"}.\n Changing the charset is not allowed since this may break encoding of special symbols"
                          _ -> False
                      )

    when dataBaseExists
      $ beforeAll_
        -- Setup: clear test space
        ( shelly $ rm_rf testWorkspaceDir >> mkdir_p testWorkspaceDir
        )
      $ describe "files"
      $ do
        let
          testConfig =
            Config
              { cfgColorMap =
                  [ ColorMap
                      { fromColor = RTFColor (Just 226) (Just 226) (Just 226)
                      , toColor = RTFColor (Just 107) (Just 0) (Just 108)
                      , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                      }
                  ]
              , cfgTextMap =
                  [ TextMap "==============================================================" "*****************************************************************************"
                  ]
              , cfgContentMap = [ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| [])]
              , cfgFontMap = [FontMap "HelveticaNeue" (FontMapFont FRoman (Just 0) "TimesNewRomanPSMT")]
              }

        it "[processDirRTF]" $ do
          let dataSrcDir = dataBasePath </> "rtf"
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
              [
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Cheatsheet- Node.js lib Express.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Node.js lib Express.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 19)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Cheatsheet- Vimperator.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Cheatsheet- Vimperator.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Command [Linux]- sysctl.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Command [Linux]- sysctl.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Modules- monad - mtl.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- monad - mtl.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Modules- testing - Hedgehog.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Modules- testing - Hedgehog.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5, 6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 9)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/Reading- 中原中也.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_Reading- 中原中也.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/18/[HTTP]- HTTP Cache.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_18_[HTTP]- HTTP Cache.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Berkley Packet Filter (BPF).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Berkley Packet Filter (BPF).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Docker registry, repository, images.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Docker registry, repository, images.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- JavaScript lib - immutable.js.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- JavaScript lib - immutable.js.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- Lua tricks.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- Lua tricks.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- React Hooks.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- React Hooks.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 2)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Cheatsheet- neo4j functions, aggregates.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Cheatsheet- neo4j functions, aggregates.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command [Linux]- resolvconf.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [Linux]- resolvconf.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command [macOS] tricks.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command [macOS] tricks.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command- dig.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- dig.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Command- ip.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Command- ip.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Notes- Windows 10.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Notes- Windows 10.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Plugin- Taglist.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Plugin- Taglist.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 1)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/Tool- Stack.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_Tool- Stack.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 3
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Network]- LDAP (Lightweight Directory Access Protocol).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- LDAP (Lightweight Directory Access Protocol).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Network]- mDNS (Multicast DNS).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Network]- mDNS (Multicast DNS).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/20/[Programming]- Language theory - Grammar.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_20_[Programming]- Language theory - Grammar.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 2)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- C CLI - gcc-nm-objdump.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- C CLI - gcc-nm-objdump.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 9)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- Homebrew.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Homebrew.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- Node.js libs.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- Node.js libs.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 13
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 12)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- bash - variables (EXPANSION).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- bash - variables (EXPANSION).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Cheatsheet- iOS - usage.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Cheatsheet- iOS - usage.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Command- mount.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- mount.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Command- w.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Command- w.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Modules- megaparsec.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- megaparsec.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4, 6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 3
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/Modules- monad - transformers.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_Modules- monad - transformers.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/[Network]- Cheatsheet.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- Cheatsheet.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/2a/[Network]- IPv4 CIDR and class network.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_2a_[Network]- IPv4 CIDR and class network.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [7]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/TestCases.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_TestCases.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Camus- Nobel lecture.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Camus- Nobel lecture.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Cheatsheet- ANSI escape code-1.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Cheatsheet- ANSI escape code-1.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Cheatsheet- Python package - networkx.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Cheatsheet- Python package - networkx.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Cheatsheet- basics - strictness, performance.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Cheatsheet- basics - strictness, performance.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Cheatsheet- gpg.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Cheatsheet- gpg.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Freud- 人はなぜ戦争をするのか.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Freud- 人はなぜ戦争をするのか.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Modules- network - servant.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Modules- network - servant.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Notes [Linux]- -proc.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Notes [Linux]- -proc.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 3
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Notes- Russia.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Notes- Russia.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/Søren Kierkegaard - Fear and Trembling.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_Søren Kierkegaard - Fear and Trembling.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 32)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/[HTTP]- Cross Origin Resource Sharing (CORS).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_[HTTP]- Cross Origin Resource Sharing (CORS).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/[Network]- IP routing.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_[Network]- IP routing.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/b/安倍晋三：美しい国 (2006).rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_b_安倍晋三：美しい国 (2006).rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/data/usage_cocoa.rtf"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTF/bak/10142023_0541_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTF_data_usage_cocoa.rtf"
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
                                    { red = Just 107
                                    , green = Just 0
                                    , blue = Just 108
                                    }
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [3, 6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont =
                                  FontMapFont
                                    { fmFamily = FRoman
                                    , fmCharset = Just 0
                                    , fmFontName = "TimesNewRomanPSMT"
                                    }
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 1)]
                    }
                )
              ]

        it "[processDirRTFD]" $ do
          let dataSrcDir = dataBasePath </> "rtfd"
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
              [
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1a/Notes- syslog.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_Notes- syslog.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1a/[Network]- Network Address Translation (NAT).rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1a_[Network]- Network Address Translation (NAT).rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1f/Cheatsheet- snort v3 pulledpork.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Cheatsheet- snort v3 pulledpork.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [3]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/1f/Concept- Numbers - Floating point.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_1f_Concept- Numbers - Floating point.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 2)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- TeX-LaTeX tricks.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- TeX-LaTeX tricks.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- basics - Template Haskell.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- basics - Template Haskell.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5, 6]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 5
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Cheatsheet- neovim LSP usage + troubleshooting.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Cheatsheet- neovim LSP usage + troubleshooting.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 2
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 4)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/Notes- Redis - Redlock.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_Notes- Redis - Redlock.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/4/[Security]- general.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_4_[Security]- general.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/Cheatsheet- gpg usage-1.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_Cheatsheet- gpg usage-1.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [5]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 4
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/Cheatsheet- tricks - editing.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Cheatsheet- tricks - editing.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 3)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/Tool- Stack usage.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_Tool- Stack usage.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/c/[Distributed systems]- CAP theorem.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_c_[Distributed systems]- CAP theorem.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 0
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [1]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Cheatsheet- macOS usage - troubleshooting.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Cheatsheet- macOS usage - troubleshooting.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Command- ssh tricks + troubleshooting.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Command- ssh tricks + troubleshooting.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , [4]
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Food- Olive oil - Balasmic Vinegar.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Food- Olive oil - Balasmic Vinegar.rtfd"
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
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 1
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ,
                ( RTFDFile
                    "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/data/e/Notes- SQL.rtfd"
                , "/Users/yuinishizawa/Projects/NotesFormatter/test_workspace/processDirRTFD/bak/10012023_0637_Users_yuinishizawa_Projects_NotesFormatter_test_workspace_processDirRTFD_data_e_Notes- SQL.rtfd"
                , ProcessResult
                    { resultMapColor =
                        [
                          ( ColorMap
                              { fromColor = RTFColor{red = Just 226, green = Just 226, blue = Just 226}
                              , toColor = RTFColor{red = Just 107, green = Just 0, blue = Just 108}
                              , toColorSpace = CSGenericRGB 41819 0 42431 Nothing
                              }
                          , []
                          )
                        ]
                    , resultMapText =
                        [
                          ( TextMap
                              { pattern = "=============================================================="
                              , replacement = "*****************************************************************************"
                              }
                          , 5
                          )
                        ]
                    , resultMapFont =
                        [
                          ( FontMap
                              { fromFontName = "HelveticaNeue"
                              , toFont = FontMapFont{fmFamily = FRoman, fmCharset = Just 0, fmFontName = "TimesNewRomanPSMT"}
                              }
                          , [0]
                          )
                        ]
                    , resultMapContent = [(ContentMap 0 (contentEscapedSequence' "85" :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ]

module Test.SpecApp (
  spec,
) where

import Control.Lens hiding ((<.>))
import Data.List (intercalate, sortOn)
import Data.List.Extra (replace)
import Data.Text.IO qualified as T
import Notes.App
import Notes.RTFDoc
import Notes.RTFFile
import Shelly hiding (path, (<.>), (</>))
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
    let config =
          ( Config
              { cfgColorMap = [ColorMap (RTFColor (Just 226) (Just 226) (Just 226)) (RTFColor (Just 230) (Just 230) (Just 230)) (CSSRGB 1 2 3 Nothing)]
              , cfgTextMap = [TextMap "a" "b"]
              , cfgContentMap = [ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| [])]
              , cfgFontMap = [FontMap "HelveticaNeue" (FontMapFont FRoman (Just 0) "TimesNewRomanPSMT")]
              }
          )
    let
      testApplyConfig :: HasCallStack => String -> RTFDoc -> ProcessResult -> Text -> Spec
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
          render doc `shouldBe` expectedText

    testApplyConfig
      "Font.rtf"
      ( RTFDoc
          ( RTFHeader
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
                          , fontFamily = FSwiss
                          , fontCharset = Just 0
                          , fontName = "ArialMT"
                          }
                    , Just
                        FontInfo
                          { fontNum = 2
                          , fontFamily = FNil
                          , fontCharset = Just 0
                          , fontName = "ComicSansMS"
                          }
                    , Just
                        FontInfo
                          { fontNum = 3
                          , fontFamily = FModern
                          , fontCharset = Just 0
                          , fontName = "CourierNewPSMT"
                          }
                    , Just
                        FontInfo
                          { fontNum = 4
                          , fontFamily = FSwiss
                          , fontCharset = Just 0
                          , fontName = "Helvetica"
                          }
                    , Just
                        FontInfo
                          { fontNum = 5
                          , fontFamily = FRoman
                          , fontCharset = Just 0
                          , fontName = "Times-Roman"
                          }
                    , Just
                        FontInfo
                          { fontNum = 6
                          , fontFamily = FRoman
                          , fontCharset = Just 0
                          , fontName = "TimesNewRomanPSMT"
                          }
                    , Just
                        FontInfo
                          { fontNum = 7
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
                  ]
              }
          )
          [ ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
          , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
          , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
          , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
          , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "qc" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlWord NoPrefix "ul" SpaceSuffix
          , ContentControlWord NoPrefix "ulc" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 1)
          , ContentControlWord NoPrefix "fs" (RTFControlParam 48)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlWord NoPrefix "ulnone" SpaceSuffix
          , ContentText "Aribl "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 2)
          , ContentText " Comic sbns\n"
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 3)
          , ContentText " Courier New"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 4)
          , ContentText " Helveticb"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " Helveticb Neue"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 5)
          , ContentText " Times\n"
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 6)
          , ContentText " Times New Rombn"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 7)
          , ContentText " Verdbnb\n"
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentText "Andble Mono"
          , ContentControlSymbol '\n'
          , ContentText "Monbco"
          ]
      )
      ( ProcessResult
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
                    , toColorSpace = CSSRGB 1 2 3 Nothing
                    }
                , []
                )
              ]
          , resultMapText =
              [
                ( TextMap
                    { pattern = "a"
                    , replacement = "b"
                    }
                , 8
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
          , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
          }
      )
      "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\froman\\fcharset0 TimesNewRomanPSMT;\\f1\\fswiss\\fcharset0 ArialMT;\\f2\\fnil\\fcharset0 ComicSansMS;\\f3\\fmodern\\fcharset0 CourierNewPSMT;\\f4\\fswiss\\fcharset0 Helvetica;\\f5\\froman\\fcharset0 Times-Roman;\\f6\\froman\\fcharset0 TimesNewRomanPSMT;\\f7\\fnil\\fcharset0 Verdana;}\n{\\colortbl;\\red255\\green255\\blue255;}\n{\\*\\expandedcolortbl;;}\n\n{\\info{\\author Yui Nishizawa}}\\vieww11520\\viewh8400\\viewkind0\\deftab720\\pard\\pardeftab720\\qc\\partightenfactor0\\f0\\fs28 \\cf0 \\ul \\ulc0 \\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f1\\fs48 \\cf0 \\ulnone Aribl \\\n\\f2 Comic sbns\n\\f0 \\\n\\f3 Courier New\\\n\\f4 Helveticb\\\n\\f0 Helveticb Neue\\\n\\f5 Times\n\\f0 \\\n\\f6 Times New Rombn\\\n\\f7 Verdbnb\n\\f0 \\\n\\\n\\\nAndble Mono\\\nMonbco}"

    testApplyConfig
      "Color.rtf"
      ( RTFDoc
          ( RTFHeader
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
                    , Just (CSSRGB 0 0 0 Nothing)
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
                    , Just (CSSRGB 1 2 3 Nothing)
                    )
                  ,
                    ( RTFColor
                        { red = Just 217
                        , green = Just 11
                        , blue = Just 5
                        }
                    , Just (CSSRGB 88946 14202 0 Nothing)
                    )
                  ,
                    ( RTFColor
                        { red = Just 217
                        , green = Just 11
                        , blue = Just 5
                        }
                    , Just (CSSRGB 88946 14202 0 (Just 50000))
                    )
                  ]
              }
          )
          [ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]], ContentControlWord NoPrefix "vieww" (RTFControlParam 11520), ContentControlWord NoPrefix "viewh" (RTFControlParam 8400), ContentControlWord NoPrefix "viewkind" (RTFControlParam 0), ContentControlWord NoPrefix "deftab" (RTFControlParam 720), ContentControlWord NoPrefix "pard" NoSuffix, ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720), ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0), ContentControlWord NoPrefix "f" (RTFControlParam 0), ContentControlWord NoPrefix "fs" (RTFControlParam 28), ContentText " ", ContentControlWord NoPrefix "cf" (RTFControlParam 2), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 3), ContentText " ", ContentControlWord NoPrefix "expnd" (RTFControlParam 0), ContentControlWord NoPrefix "expndtw" (RTFControlParam 0), ContentControlWord NoPrefix "kerning" (RTFControlParam 0), ContentControlSymbol '\n', ContentControlSymbol '\n', ContentControlWord NoPrefix "pard" NoSuffix, ContentControlWord NoPrefix "tx" (RTFControlParam 566), ContentControlWord NoPrefix "tx" (RTFControlParam 1133), ContentControlWord NoPrefix "tx" (RTFControlParam 1700), ContentControlWord NoPrefix "tx" (RTFControlParam 2267), ContentControlWord NoPrefix "tx" (RTFControlParam 2834), ContentControlWord NoPrefix "tx" (RTFControlParam 3401), ContentControlWord NoPrefix "tx" (RTFControlParam 3968), ContentControlWord NoPrefix "tx" (RTFControlParam 4535), ContentControlWord NoPrefix "tx" (RTFControlParam 5102), ContentControlWord NoPrefix "tx" (RTFControlParam 5669), ContentControlWord NoPrefix "tx" (RTFControlParam 6236), ContentControlWord NoPrefix "tx" (RTFControlParam 6803), ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720), ContentControlWord NoPrefix "slleading" (RTFControlParam 24), ContentControlWord NoPrefix "pardirnatural" NoSuffix, ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0), ContentControlWord NoPrefix "f" (RTFControlParam 1), ContentText " ", ContentControlWord NoPrefix "cf" (RTFControlParam 2), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 4), ContentText " ", ContentControlSymbol '\n', ContentText "Code block", ContentControlSymbol '\n', ContentControlSymbol '\n', ContentControlWord NoPrefix "f" (RTFControlParam 0), ContentText " ", ContentControlWord NoPrefix "cf" (RTFControlParam 0), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 1), ContentText " ", ContentControlWord NoPrefix "kerning" (RTFControlParam 1), ContentControlWord NoPrefix "expnd" (RTFControlParam 0), ContentControlWord NoPrefix "expndtw" (RTFControlParam 0), ContentText " ", ContentControlSymbol '\n', ContentControlWord NoPrefix "cf" (RTFControlParam 5), ContentText " Red text", ContentControlSymbol '\n', ContentControlSymbol '\n', ContentControlWord NoPrefix "f" (RTFControlParam 2), ContentControlWord NoPrefix "b" SpaceSuffix, ContentControlWord NoPrefix "cf" (RTFControlParam 2), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 6), ContentText " Red with blphb\n", ContentControlWord NoPrefix "f" (RTFControlParam 0), ContentControlWord NoPrefix "b" (RTFControlParam 0), ContentText " ", ContentControlWord NoPrefix "cf" (RTFControlParam 5), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 1), ContentText " ", ContentControlSymbol '\n', ContentControlSymbol '\n', ContentControlWord NoPrefix "cf" (RTFControlParam 2), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 5), ContentText " Red highlight text", ContentControlWord NoPrefix "cf" (RTFControlParam 0), ContentText " ", ContentControlWord NoPrefix "cb" (RTFControlParam 1), ContentText " ", ContentControlSymbol '\n', ContentControlSymbol '\n']
      )
      ( ProcessResult
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
                    , toColorSpace = CSSRGB 1 2 3 Nothing
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
          , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
          }
      )
      "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\froman\\fcharset0 TimesNewRomanPSMT;\\f1\\fnil\\fcharset0 Monaco;\\f2\\fnil\\fcharset0 HelveticaNeue-Bold;}\n{\\colortbl;\\red255\\green255\\blue255;\\red0\\green0\\blue0;\\red255\\green255\\blue255;\\red230\\green230\\blue230;\\red217\\green11\\blue5;\\red217\\green11\\blue5;}\n{\\*\\expandedcolortbl;;\\cssrgb\\c0\\c0\\c0;\\csgray\\c100000;\\cssrgb\\c1\\c2\\c3;\\cssrgb\\c88946\\c14202\\c0;\\cssrgb\\c88946\\c14202\\c0\\c50000;}\n\n{\\info{\\author Yui Nishizawa}}\\vieww11520\\viewh8400\\viewkind0\\deftab720\\pard\\pardeftab720\\partightenfactor0\\f0\\fs28 \\cf2 \\cb3 \\expnd0\\expndtw0\\kerning0\\\n\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f1 \\cf2 \\cb4 \\\nCode block\\\n\\\n\\f0 \\cf0 \\cb1 \\kerning1\\expnd0\\expndtw0 \\\n\\cf5 Red text\\\n\\\n\\f2\\b \\cf2 \\cb6 Red with blphb\n\\f0\\b0 \\cf5 \\cb1 \\\n\\\n\\cf2 \\cb5 Red highlight text\\cf0 \\cb1 \\\n\\\n}"

    testApplyConfig
      "Styles.rtf"
      ( RTFDoc
          ( RTFHeader
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
                    , Just (CSSRGB 0 0 0 Nothing)
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
                    , Just (CSSRGB 1 2 3 Nothing)
                    )
                  ]
              }
          )
          [ ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
          , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
          , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
          , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
          , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "cb" (RTFControlParam 3)
          , ContentText " "
          , ContentControlWord NoPrefix "expnd" (RTFControlParam 0)
          , ContentControlWord NoPrefix "expndtw" (RTFControlParam 0)
          , ContentControlWord NoPrefix "kerning" (RTFControlParam 0)
          , ContentText "Normbl text"
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "cb" (RTFControlParam 4)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentText "Code block"
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlWord NoPrefix "cb" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "kerning" (RTFControlParam 1)
          , ContentControlWord NoPrefix "expnd" (RTFControlParam 0)
          , ContentControlWord NoPrefix "expndtw" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 2)
          , ContentControlWord NoPrefix "b" SpaceSuffix
          , ContentText "Bold text\n"
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentControlWord NoPrefix "b" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "ul" SpaceSuffix
          , ContentText "Underline text"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "ulnone" SpaceSuffix
          , ContentControlWord NoPrefix "strike" SpaceSuffix
          , ContentControlWord NoPrefix "strikec" (RTFControlParam 0)
          , ContentText " Strikethrough"
          , ContentControlWord NoPrefix "strike" (RTFControlParam 0)
          , ContentControlWord NoPrefix "striked" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "fs" (RTFControlParam 96)
          , ContentText " size 48"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "fs" (RTFControlParam 24)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "qj" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " justify"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " left blign"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "qc" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " center blign"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "qr" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " right blign\n"
          , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          ]
      )
      ( ProcessResult
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
                    , toColorSpace = CSSRGB 1 2 3 Nothing
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
                , 4
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
          , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
          }
      )
      "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\froman\\fcharset0 TimesNewRomanPSMT;\\f1\\fnil\\fcharset0 Monaco;\\f2\\fnil\\fcharset0 HelveticaNeue-Bold;}\n{\\colortbl;\\red255\\green255\\blue255;\\red0\\green0\\blue0;\\red255\\green255\\blue255;\\red230\\green230\\blue230;}\n{\\*\\expandedcolortbl;;\\cssrgb\\c0\\c0\\c0;\\csgray\\c100000;\\cssrgb\\c1\\c2\\c3;}\n\n{\\info{\\author Yui Nishizawa}}\\vieww11520\\viewh8400\\viewkind0\\deftab720\\pard\\pardeftab720\\partightenfactor0\\f0\\fs28 \\cf2 \\cb3 \\expnd0\\expndtw0\\kerning0Normbl text\\\n\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f1 \\cf2 \\cb4 \\\nCode block\\\n\\\n\\f0 \\cf0 \\cb1 \\kerning1\\expnd0\\expndtw0 \\\n\\\n\\f2\\b Bold text\n\\f0\\b0 \\\n\\ul Underline text\\\n\\ulnone \\strike \\strikec0 Strikethrough\\strike0\\striked0 \\\n\\\n\\fs96 size 48\\\n\\fs24 \\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\qj\\partightenfactor0\\cf0 justify\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 left blign\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\qc\\partightenfactor0\\cf0 center blign\\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\qr\\partightenfactor0\\cf0 right blign\n\\fs28 \\\n\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 \\\n}"

    testApplyConfig
      "SpecialCharacters.rtf"
      ( RTFDoc
          ( RTFHeader
              { rtfCharset = Ansi 1252
              , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
              , rtfFontTbl =
                  FontTbl
                    [ Just
                        FontInfo
                          { fontNum = 0
                          , fontFamily = FNil
                          , fontCharset = Just 128
                          , fontName = "HiraginoSans-W3"
                          }
                    , Just
                        FontInfo
                          { fontNum = 1
                          , fontFamily = FRoman
                          , fontCharset = Just 0
                          , fontName = "TimesNewRomanPSMT"
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
                  ]
              }
          )
          [ ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
          , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
          , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
          , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
          , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , -- 日本語
            ContentEscapedSequence 147
          , ContentEscapedSequence 250
          , ContentEscapedSequence 150
          , ContentEscapedSequence 123
          , ContentEscapedSequence 140
          , ContentEscapedSequence 234
          , ContentText "\n"
          , ContentControlWord NoPrefix "f" (RTFControlParam 1)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentText " "
          , -- あいうえお
            ContentEscapedSequence 130
          , ContentEscapedSequence 160
          , ContentEscapedSequence 130
          , ContentEscapedSequence 162
          , ContentEscapedSequence 130
          , ContentEscapedSequence 164
          , ContentEscapedSequence 130
          , ContentEscapedSequence 166
          , ContentEscapedSequence 130
          , ContentEscapedSequence 168
          , ContentControlSymbol '\n'
          , -- かきくけこ
            ContentEscapedSequence 130
          , ContentEscapedSequence 169
          , ContentEscapedSequence 130
          , ContentEscapedSequence 171
          , ContentEscapedSequence 130
          , ContentEscapedSequence 173
          , ContentEscapedSequence 130
          , ContentEscapedSequence 175
          , ContentEscapedSequence 130
          , ContentEscapedSequence 177
          , ContentControlSymbol '\n'
          , -- ...
            ContentControlWord NoPrefix "f" (RTFControlParam 1)
          , ContentText "..."
          , ContentText ";"
          ]
      )
      ( ProcessResult
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
                    , toColorSpace = CSSRGB 1 2 3 Nothing
                    }
                , []
                )
              ]
          , resultMapText =
              [
                ( TextMap
                    { pattern = "a"
                    , replacement = "b"
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
          , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 1)]
          }
      )
      "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\fnil\\fcharset128 HiraginoSans-W3;\\f1\\froman\\fcharset0 TimesNewRomanPSMT;}\n{\\colortbl;\\red255\\green255\\blue255;}\n{\\*\\expandedcolortbl;;}\n\n{\\info{\\author Yui Nishizawa}}\\vieww11520\\viewh8400\\viewkind0\\deftab720\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f0\\fs28 \\cf0 \\\n\\'93\\'fa\\'96\\'7b\\'8c\\'ea\n\\f1 \\\n\\f0 \\'82\\'a0\\'82\\'a2\\'82\\'a4\\'82\\'a6\\'82\\'a8\\\n\\'82\\'a9\\'82\\'ab\\'82\\'ad\\'82\\'af\\'82\\'b1\\\n\\f1...;}"

    testApplyConfig
      "Table.rtf"
      ( RTFDoc
          ( RTFHeader
              { rtfCharset = Ansi 1252
              , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
              , rtfFontTbl =
                  FontTbl
                    [ Just
                        FontInfo
                          { fontNum = 0
                          , fontFamily = FNil
                          , fontCharset = Just 128
                          , fontName = "HiraginoSans-W3"
                          }
                    , Just
                        FontInfo
                          { fontNum = 1
                          , fontFamily = FRoman
                          , fontCharset = Just 0
                          , fontName = "TimesNewRomanPSMT"
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
                        { red = Just 191
                        , green = Just 191
                        , blue = Just 191
                        }
                    , Just (CSGray 79525)
                    )
                  ]
              }
          )
          [ ContentGroup
              [ContentControlWord StarPrefix "listtable" NoSuffix, ContentGroup [ContentControlWord NoPrefix "list" NoSuffix, ContentControlWord NoPrefix "listtemplateid" (RTFControlParam 1), ContentControlWord NoPrefix "listhybrid" NoSuffix, ContentGroup [ContentControlWord NoPrefix "listlevel" NoSuffix, ContentControlWord NoPrefix "levelnfc" (RTFControlParam 23), ContentControlWord NoPrefix "levelnfcn" (RTFControlParam 23), ContentControlWord NoPrefix "leveljc" (RTFControlParam 0), ContentControlWord NoPrefix "leveljcn" (RTFControlParam 0), ContentControlWord NoPrefix "levelfollow" (RTFControlParam 0), ContentControlWord NoPrefix "levelstartat" (RTFControlParam 1), ContentControlWord NoPrefix "levelspace" (RTFControlParam 360), ContentControlWord NoPrefix "levelindent" (RTFControlParam 0), ContentGroup [ContentControlWord StarPrefix "levelmarker" SpaceSuffix, ContentControlSymbol '{', ContentText "disc", ContentControlSymbol '}'], ContentGroup [ContentControlWord NoPrefix "leveltext" NoSuffix, ContentControlWord NoPrefix "leveltemplateid" (RTFControlParam 1), ContentEscapedSequence 1, ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " ;"], ContentGroup [ContentControlWord NoPrefix "levelnumbers" NoSuffix, ContentText ";"], ContentControlWord NoPrefix "fi" (RTFControlParam (-360)), ContentControlWord NoPrefix "li" (RTFControlParam 720), ContentControlWord NoPrefix "lin" (RTFControlParam 720), ContentText " "], ContentGroup [ContentControlWord NoPrefix "listlevel" NoSuffix, ContentControlWord NoPrefix "levelnfc" (RTFControlParam 23), ContentControlWord NoPrefix "levelnfcn" (RTFControlParam 23), ContentControlWord NoPrefix "leveljc" (RTFControlParam 0), ContentControlWord NoPrefix "leveljcn" (RTFControlParam 0), ContentControlWord NoPrefix "levelfollow" (RTFControlParam 0), ContentControlWord NoPrefix "levelstartat" (RTFControlParam 1), ContentControlWord NoPrefix "levelspace" (RTFControlParam 360), ContentControlWord NoPrefix "levelindent" (RTFControlParam 0), ContentGroup [ContentControlWord StarPrefix "levelmarker" SpaceSuffix, ContentControlSymbol '{', ContentText "hyphen", ContentControlSymbol '}'], ContentGroup [ContentControlWord NoPrefix "leveltext" NoSuffix, ContentControlWord NoPrefix "leveltemplateid" (RTFControlParam 2), ContentEscapedSequence 1, ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " ;"], ContentGroup [ContentControlWord NoPrefix "levelnumbers" NoSuffix, ContentText ";"], ContentControlWord NoPrefix "fi" (RTFControlParam (-360)), ContentControlWord NoPrefix "li" (RTFControlParam 1440), ContentControlWord NoPrefix "lin" (RTFControlParam 1440), ContentText " "], ContentGroup [ContentControlWord NoPrefix "listname" SpaceSuffix, ContentText ";"], ContentControlWord NoPrefix "listid" (RTFControlParam 1)]]
          , ContentGroup [ContentControlWord StarPrefix "listoverridetable" NoSuffix, ContentGroup [ContentControlWord NoPrefix "listoverride" NoSuffix, ContentControlWord NoPrefix "listid" (RTFControlParam 1), ContentControlWord NoPrefix "listoverridecount" (RTFControlParam 0), ContentControlWord NoPrefix "ls" (RTFControlParam 1)]]
          , ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
          , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
          , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
          , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
          , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 0)
          , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentText "tbble"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "trowd" SpaceSuffix
          , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
          , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
          , ContentText " "
          , ContentControlWord NoPrefix "trbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "trbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "trbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "f" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " Column A"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " Column B"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "row" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "trowd" SpaceSuffix
          , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
          , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
          , ContentText " "
          , ContentControlWord NoPrefix "trbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "trbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " Row 1"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " 1"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " 2"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "row" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "trowd" SpaceSuffix
          , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
          , ContentText " "
          , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
          , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
          , ContentText " "
          , ContentControlWord NoPrefix "trbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "trbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "trbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
          , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
          , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
          , ContentControlWord NoPrefix "clbrdrt" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrl" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrb" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clbrdrr" NoSuffix
          , ContentControlWord NoPrefix "brdrs" NoSuffix
          , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
          , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
          , ContentText " "
          , ContentControlWord NoPrefix "gaph" NoSuffix
          , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " Row 2"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " 3"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "intbl" NoSuffix
          , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " 4"
          , ContentControlWord NoPrefix "cell" SpaceSuffix
          , ContentControlWord NoPrefix "lastrow" NoSuffix
          , ContentControlWord NoPrefix "row" NoSuffix
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentControlSymbol '\n'
          , ContentText "list"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 220)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 720)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "li" (RTFControlParam 720)
          , ContentControlWord NoPrefix "fi" (RTFControlParam (-720))
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
          , ContentControlWord NoPrefix "ilvl" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " \t"]
          , ContentText "item1"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 940)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1440)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "li" (RTFControlParam 1440)
          , ContentControlWord NoPrefix "fi" (RTFControlParam (-1440))
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
          , ContentControlWord NoPrefix "ilvl" (RTFControlParam 1)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
          , ContentText "subitem 1b"
          , ContentControlSymbol '\n'
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 220)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 720)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "li" (RTFControlParam 720)
          , ContentControlWord NoPrefix "fi" (RTFControlParam (-720))
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
          , ContentControlWord NoPrefix "ilvl" (RTFControlParam 0)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " \t"]
          , ContentText "item2"
          , ContentControlSymbol '\n'
          , ContentControlWord NoPrefix "pard" NoSuffix
          , ContentControlWord NoPrefix "tx" (RTFControlParam 940)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1440)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
          , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
          , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
          , ContentControlWord NoPrefix "li" (RTFControlParam 1440)
          , ContentControlWord NoPrefix "fi" (RTFControlParam (-1440))
          , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
          , ContentControlWord NoPrefix "pardirnatural" NoSuffix
          , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
          , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
          , ContentControlWord NoPrefix "ilvl" (RTFControlParam 1)
          , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
          , ContentText " "
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
          , ContentText "subitem 2b"
          , ContentControlSymbol '\n'
          , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
          , ContentControlSymbol '\n'
          ]
      )
      ( ProcessResult
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
                    , toColorSpace = CSSRGB 1 2 3 Nothing
                    }
                , []
                )
              ]
          , resultMapText =
              [
                ( TextMap
                    { pattern = "a"
                    , replacement = "b"
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
          , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
          }
      )
      "{\\rtf1\\ansi\\ansicpg1252\\cocoartf2639\\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\fnil\\fcharset128 HiraginoSans-W3;\\f1\\froman\\fcharset0 TimesNewRomanPSMT;}\n{\\colortbl;\\red255\\green255\\blue255;\\red191\\green191\\blue191;}\n{\\*\\expandedcolortbl;;\\csgray\\c79525;}\n\n{\\*\\listtable{\\list\\listtemplateid1\\listhybrid{\\listlevel\\levelnfc23\\levelnfcn23\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{disc\\}}{\\leveltext\\leveltemplateid1\\'01\\uc0\\u8226 ;}{\\levelnumbers;}\\fi-360\\li720\\lin720 }{\\listlevel\\levelnfc23\\levelnfcn23\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{hyphen\\}}{\\leveltext\\leveltemplateid2\\'01\\uc0\\u8259 ;}{\\levelnumbers;}\\fi-360\\li1440\\lin1440 }{\\listname ;}\\listid1}}{\\*\\listoverridetable{\\listoverride\\listid1\\listoverridecount0\\ls1}}{\\info{\\author Yui Nishizawa}}\\vieww11520\\viewh8400\\viewkind0\\deftab720\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f0\\fs28 \\cf0 \\\ntbble\\\n\\itap1\\trowd \\taflags1 \\trgaph108\\trleft-108 \\trbrdrt\\brdrnil \\trbrdrl\\brdrnil \\trbrdrr\\brdrnil \\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx2880\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx5760\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx8640\\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\f1 \\cf0 \\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 Column A\\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 Column B\\cell \\row\\itap1\\trowd \\taflags1 \\trgaph108\\trleft-108 \\trbrdrl\\brdrnil \\trbrdrr\\brdrnil \\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx2880\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx5760\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx8640\\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 Row 1\\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 1\\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 2\\cell \\row\\itap1\\trowd \\taflags1 \\trgaph108\\trleft-108 \\trbrdrl\\brdrnil \\trbrdrt\\brdrnil \\trbrdrr\\brdrnil \\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx2880\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx5760\\clvertalc \\clshdrawnil \\clbrdrt\\brdrs\\brdrw20\\brdrcf2 \\clbrdrl\\brdrs\\brdrw20\\brdrcf2 \\clbrdrb\\brdrs\\brdrw20\\brdrcf2 \\clbrdrr\\brdrs\\brdrw20\\brdrcf2 \\clpadl100 \\clpadr100 \\gaph\\cellx8640\\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 Row 2\\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 3\\cell \\pard\\intbl\\itap1\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 4\\cell \\lastrow\\row\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\slleading24\\pardirnatural\\partightenfactor0\\cf0 \\\nlist\\\n\\pard\\tx220\\tx720\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\li720\\fi-720\\slleading24\\pardirnatural\\partightenfactor0\\ls1\\ilvl0\\cf0 {\\listtext\t\\uc0\\u8226 \t}item1\\\n\\pard\\tx940\\tx1440\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\li1440\\fi-1440\\slleading24\\pardirnatural\\partightenfactor0\\ls1\\ilvl1\\cf0 {\\listtext\t\\uc0\\u8259 \t}subitem 1b\\\n{\\listtext\t\\uc0\\u8259 \t}\\\n\\pard\\tx220\\tx720\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\li720\\fi-720\\slleading24\\pardirnatural\\partightenfactor0\\ls1\\ilvl0\\cf0 {\\listtext\t\\uc0\\u8226 \t}item2\\\n\\pard\\tx940\\tx1440\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardeftab720\\li1440\\fi-1440\\slleading24\\pardirnatural\\partightenfactor0\\ls1\\ilvl1\\cf0 {\\listtext\t\\uc0\\u8259 \t}subitem 2b\\\n{\\listtext\t\\uc0\\u8259 \t}\\\n}"

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
              , cfgContentMap = [ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| [])]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 19)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 9)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 2)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 1)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 2)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 9)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 12)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 32)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 1)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 2)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 4)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 3)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
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
                    , resultMapContent = [(ContentMap (ContentEscapedSequence 133 :| []) (ContentText "..." :| []), 0)]
                    }
                )
              ]

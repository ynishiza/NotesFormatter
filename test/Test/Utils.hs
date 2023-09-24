module Test.Utils (
  multiline,
  rtfPath,
  normalizeRTFForTest,
  expectToRTFDocSuccess,
  runAppTest,
  dataPath,
  prependToFileName,
  makePathRelativeTo,
  makeRTFFilePathRelativeTo,
  module X,
) where

import Data.Text qualified as T
import Language.Haskell.TH.Quote
import Notes.App
import Notes.RTFDoc as X
import System.FilePath
import Test.Hspec
import Notes.RTFFile
import Data.List.Extra (replace)

runAppTest :: App a -> IO a
runAppTest app = do
  appConfig <- mkAppOtions backupPath testConfig
  runApp logPath appConfig app
 where
  logPath = "testlog.txt"
  backupPath = "testbak"
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

prependToFileName :: String -> FilePath -> FilePath
prependToFileName timestamp path = joinPath [takeDirectory path, newFileName]
 where
  newFileName = timestamp <> "_" <> takeFileName path

makePathRelativeTo :: FilePath -> FilePath -> FilePath
makePathRelativeTo base = replace base ""

makeRTFFilePathRelativeTo :: FilePath -> RTFFile filetype -> RTFFile filetype
makeRTFFilePathRelativeTo base (RTFFile path) = RTFFile (makePathRelativeTo base path)
makeRTFFilePathRelativeTo base (RTFDFile path) = RTFDFile (makePathRelativeTo base path)

rtfPath :: FilePath
rtfPath = dataPath </> "rtf"

dataPath :: FilePath
dataPath = basePath </> "data"

multiline :: QuasiQuoter
multiline =
  QuasiQuoter
    { quoteExp = \s -> [|T.pack s|]
    , quoteDec = undefined
    , quoteType = undefined
    , quotePat = undefined
    }

-- Note: newlines are insignificant in RTF.
-- Hence, normalize the RTF text by removing new lines.
-- However, the content text may have newlines, represented as an
-- RTF symbol
--      \\n
-- To avoid replacing the newline RTF symbol, replace the symbol with a placeholder first.
normalizeRTFForTest :: Text -> Text
normalizeRTFForTest text =
  text
    -- newlinePlaceholder + backslashPlaceholder
    --
    -- We want to remove insignificant newlines.
    -- However, genuine newlines are represented by an RTF symbol
    -- i.e.
    --      newline RTF symbol    \\n
    --
    --  Thus, in order to avoid removing the newline RTF symbol, we replace it with a placeholder first.
    --
    --  However, when replacing with the newline placeholder, we also need to distinguish between
    --  the backslash RTF symbol followed by an insignificant newline and the newline symbol
    --  i.e.
    --     newline symbol                       \\n         should not be removed
    --     backslash RTF symbol + newline       \\\n        should be removed
    --
    --  Thus, to distinguish between the 2 cases above, we also need to replace the backslash RTF symbol with a placeholder.
    & T.replace "\\\\" backslashPlaceholder -- TODO: WHY is this needed? Otherwise "TeX-LaTeX tricks.rtfd" fails
    & T.replace "\\\n" newlinePlaceholder
    & T.replace "\n" ""
    & T.replace "\r" ""
    -- Note: use placeholder for whitespace to highlight diff more clearly.
    & T.replace " " whitespacePlaholder
    & T.replace newlinePlaceholder "\n"
    & T.replace backslashPlaceholder "\\\\"
 where
  whitespacePlaholder = " _ "
  newlinePlaceholder = "😄"
  backslashPlaceholder = "😅"

expectToRTFDocSuccess :: ToRTFDoc a => Text -> IO a
expectToRTFDocSuccess d = do
  let result = parseDoc_ toRTFDoc d
  case result of
    Left msg -> do
      expectationFailure $ show msg
      fail $ show msg
    Right v -> return v

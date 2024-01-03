module Test.Utils (
  rtfPath,
  normalizeRTFForTest,
  expectToRTFDocSuccess,
  dataPath,
  dataBasePath,
  dataBaseExists,
  prependToFileName,
  changePathBase,
  changeRTFFilePathBase,
  module X,
) where

import Data.Text qualified as T
import Language.Haskell.TH
import Notes.App
import Notes.RTFDoc as X
import Notes.RTFFile
import System.Directory
import System.FilePath
import Test.Hspec hiding (runIO)

prependToFileName :: String -> FilePath -> FilePath
prependToFileName timestamp path = joinPath [takeDirectory path, newFileName]
 where
  newFileName = timestamp <> "_" <> takeFileName path

changePathBase :: FilePath -> FilePath -> FilePath -> FilePath
changePathBase oldBase newBase path = newBase </> makeRelative oldBase path

changeRTFFilePathBase :: FilePath -> FilePath -> RTFFile filetype -> RTFFile filetype
changeRTFFilePathBase oldBase newBase (RTFFile path) = RTFFile (changePathBase oldBase newBase path)
changeRTFFilePathBase oldBase newBase (RTFDFile path) = RTFDFile (changePathBase oldBase newBase path)

rtfPath :: FilePath
rtfPath = dataPath </> "rtf"

dataPath :: FilePath
dataPath = basePath </> "data"

dataBasePath :: FilePath
dataBasePath = dataPath </> "database"

dataBaseExists :: Bool
dataBaseExists =
  $( do
      x <- runIO (doesDirectoryExist $ basePath </> "data" </> "database")
      if x then [|True|] else [|False|]
   )

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

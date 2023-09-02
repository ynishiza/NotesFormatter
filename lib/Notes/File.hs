module Notes.File (
  extensionRTF,
  extensionRTFD,
  readRTF,
  readRTFD,
  listAllFiles,
  listFiles,
  isExtensionOf,
  isRTF,
  isRTFD,
) where

import System.FilePath

import Control.Exception (throw)
import Control.Monad
import Data.ByteString qualified as B
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding
import System.Directory
import System.PosixCompat

isRTF :: FilePath -> Bool
isRTF = isExtensionOf extensionRTF

isRTFD :: FilePath -> Bool
isRTFD = isExtensionOf extensionRTFD

extensionRTF :: IsString s => s
extensionRTF = "rtf"

extensionRTFD :: IsString s => s
extensionRTFD = "rtfd"

readRTF :: FilePath -> IO Text
readRTF path = do
  validateFile path extensionRTF
  decodeUtf8 <$> B.readFile path

readRTFD :: FilePath -> IO Text
readRTFD path = do
  validateFile path extensionRTFD
  let rtfPath = path </> "TXT.rtf"
  validateFile rtfPath extensionRTF
  decodeLatin1 <$> B.readFile rtfPath

validateFile :: FilePath -> String -> IO ()
validateFile path extension = do
  exists <- fileExist path
  unless exists $ throw $ userError $ "File not found exist:" <> path
  unless (isExtensionOf extension path) $ throw $ userError $ "Wrong extension:" <> path <> "\nExpected:" <> extension

listFiles :: (FilePath -> Bool -> Bool) -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
listFiles isMatch shouldSearchDir = makeAbsolute >=> go
 where
  go :: FilePath -> IO [FilePath]
  go dirPath = do
    isDir <- doesDirectoryExist dirPath
    if isMatch dirPath isDir
      then return [dirPath]
      else do
        if isDir && shouldSearchDir dirPath
          then do
            content <- listDirectory dirPath
            join <$> traverse (go . (dirPath </>)) content
          else return []

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles = listFiles (\_ isDir -> not isDir) (const True)

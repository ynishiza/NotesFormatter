module Notes.RTFFile (
  RTFFile (..),
  SomeRTFFile (..),
  rtfFromPath,
  rtfFile,
  rtfdFile,
  FileType (..),
  SFileType (..),
  readRTFFile,
  writeRTFFile,
  rtfFilePath,
  copyRTFFile,
  RTFSym0,
  RTFDSym0,
) where

import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Singletons.TH
import Data.Text (Text)
import Notes.File.RTF
import System.Directory (copyFile)

data FileType = RTF | RTFD deriving stock (Eq, Show)

genSingletons [''FileType]

type RTFFile :: FileType -> Type
data RTFFile filetype where
  RTFFile :: SFileType filetype -> FilePath -> RTFFile filetype

instance Show (RTFFile filetype) where
  show (RTFFile SRTF path) = "RTFFile SRTF \"" <> path <> "\""
  show (RTFFile SRTFD path) = "RTFFile SRTFD \"" <> path <> "\""

instance Eq (RTFFile filetype) where
  (RTFFile _ p1) == (RTFFile _ p2) = p1 == p2

data SomeRTFFile = forall filetype. SomeRTFFile (RTFFile filetype)

instance Show SomeRTFFile where show (SomeRTFFile f) = "SomeRTFFile " <> show f

instance Eq SomeRTFFile where
  (SomeRTFFile (RTFFile SRTF path1)) == (SomeRTFFile (RTFFile SRTF path2)) = path1 == path2
  (SomeRTFFile (RTFFile SRTFD path1)) == (SomeRTFFile (RTFFile SRTFD path2)) = path1 == path2
  _ == _ = False

rtfFromPath :: FilePath -> SomeRTFFile
rtfFromPath filePath
  | isRTF filePath = SomeRTFFile $ rtfFile filePath
  | isRTFD filePath = SomeRTFFile $ rtfdFile filePath
  | otherwise = error $ "Not an RTF or RTFD file " <> filePath

rtfFile :: FilePath -> RTFFile 'RTF
rtfFile path = if isRTF path then RTFFile SRTF path else error ("Invalid RTF path:" <> path)

rtfdFile :: FilePath -> RTFFile 'RTFD
rtfdFile path = if isRTFD path then RTFFile SRTFD path else error ("Invalid RTFD path:" <> path)

rtfFilePath :: forall filetype. RTFFile filetype -> FilePath
rtfFilePath (RTFFile SRTF path) = path
rtfFilePath (RTFFile SRTFD path) = path </> fileRTFDTXT

readRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> m Text
readRTFFile (RTFFile SRTF path) = readRTF path
readRTFFile (RTFFile SRTFD path) = do
  validateFile path extensionRTFD
  readRTF $ path </> fileRTFDTXT

writeRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> Text -> m ()
writeRTFFile (RTFFile SRTF path) text = writeRTF path text
writeRTFFile (RTFFile SRTFD path) text = do
  validateFilePath path extensionRTFD
  let rtfPath = path </> fileRTFDTXT
  writeRTF rtfPath text

copyRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> FilePath -> m ()
copyRTFFile (RTFFile SRTF path) dstPath = liftIO $ copyFile path dstPath
copyRTFFile (RTFFile SRTFD path) dstPath = liftIO $ copyFilesRecursive path dstPath

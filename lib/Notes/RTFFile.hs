module Notes.RTFFile (
  RTFFile (..),
  rtfFile,
  rtfdFile,
  FileType (..),
  readRTFFile,
  writeRTFFile,
  rtfFilePath,
  copyRTFFile,
  RTFSym0,
  RTFDSym0,
) where

import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import Notes.File.RTF
import System.Directory (copyFile)

data FileType = RTF | RTFD deriving stock (Eq, Show)

genSingletons [''FileType]

type RTFFile :: FileType -> Type
data RTFFile filetype where
  RTFFile :: forall filetype. SingI filetype => FilePath -> RTFFile filetype

instance Show (RTFFile filetype) where
  show (RTFFile path) = withSing @filetype $ \case
    SRTF -> "rft:" <> path
    SRTFD -> "rtfd:" <> path

rtfFile :: FilePath -> RTFFile 'RTF
rtfFile path = if isRTF path then RTFFile path else error ("Invalid RTF path:" <> path)

rtfdFile :: FilePath -> RTFFile 'RTFD
rtfdFile path = if isRTFD path then RTFFile path else error ("Invalid RTFD path:" <> path)

rtfFilePath :: forall filetype. RTFFile filetype -> FilePath
rtfFilePath (RTFFile path) = withSing @filetype $ \case
  SRTF -> path
  SRTFD -> path </> fileRTFDTXT

readRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> m Text
readRTFFile (RTFFile path) = withSing @filetype $ \case
  SRTF -> liftIO (readRTF path)
  SRTFD -> liftIO (readRTFD path)

writeRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> Text -> m ()
writeRTFFile (RTFFile path) text = withSing @filetype $ \case
  SRTF -> writeRTF path text
  SRTFD -> writeRTFD path text

copyRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> FilePath -> m ()
copyRTFFile (RTFFile path) dstPath = withSing @filetype $ \case
  SRTF -> liftIO $ copyFile path dstPath
  SRTFD -> liftIO $ copyFilesRecursive path dstPath

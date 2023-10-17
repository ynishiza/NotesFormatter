{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

module Notes.RTFFile (
  RTFFile,
  pattern RTFFile,
  pattern RTFDFile,
  pattern AnyRTFFile,
  SomeRTFFile (..),
  withSomeRTFFile,
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
  _RTFFileBase,
) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Singletons.TH
import Notes.File.RTF
import Notes.Utils
import System.Directory (copyFile)

data FileType = RTF | RTFD deriving stock (Eq, Show)

genSingletons [''FileType]

type RTFFile :: FileType -> Type
data RTFFile filetype where
  RTFFileBase :: SFileType filetype -> FilePath -> RTFFile filetype

$(makePrisms ''RTFFile)
$(makeLensesWith dataLensRules ''RTFFile)

pattern RTFFile :: () => (filetype ~ 'RTF) => FilePath -> RTFFile filetype
pattern RTFFile path = RTFFileBase SRTF path

pattern RTFDFile :: () => (filetype ~ 'RTFD) => FilePath -> RTFFile filetype
pattern RTFDFile path = RTFFileBase SRTFD path
{-# COMPLETE RTFFile, RTFDFile #-}

pattern AnyRTFFile :: () => FilePath -> RTFFile filetype
pattern AnyRTFFile path <- RTFFileBase _ path
{-# COMPLETE AnyRTFFile #-}

instance Show (RTFFile filetype) where
  show (RTFFile path) = "RTFFile \"" <> path <> "\""
  show (RTFDFile path) = "RTFDFile \"" <> path <> "\""

instance Eq (RTFFile filetype) where
  (AnyRTFFile p1) == (AnyRTFFile p2) = p1 == p2

data SomeRTFFile = forall filetype. SomeRTFFile (RTFFile filetype)

withSomeRTFFile :: (forall t. RTFFile t -> a) -> SomeRTFFile -> a
withSomeRTFFile f (SomeRTFFile file) = f file

instance Show SomeRTFFile where show (SomeRTFFile f) = "SomeRTFFile " <> show f

instance Eq SomeRTFFile where
  (SomeRTFFile (RTFFile path1)) == (SomeRTFFile (RTFFile path2)) = path1 == path2
  (SomeRTFFile (RTFDFile path1)) == (SomeRTFFile (RTFDFile path2)) = path1 == path2
  _ == _ = False

rtfFromPath :: FilePath -> SomeRTFFile
rtfFromPath filePath
  | isRTF filePath = SomeRTFFile $ rtfFile filePath
  | isRTFD filePath = SomeRTFFile $ rtfdFile filePath
  | otherwise = error $ "Not an RTF or RTFD file " <> filePath

rtfFile :: FilePath -> RTFFile 'RTF
rtfFile path
  | isRTF path = RTFFile path
  | otherwise = error ("Invalid RTF path:" <> path)

rtfdFile :: FilePath -> RTFFile 'RTFD
rtfdFile path
  | isRTFD path = RTFDFile path
  | otherwise = error ("Invalid RTFD path:" <> path)

rtfFilePath :: forall filetype. RTFFile filetype -> FilePath
rtfFilePath (RTFFile path) = path
rtfFilePath (RTFDFile path) = path </> fileRTFDTXT

readRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> m Text
readRTFFile (RTFFile path) = readRTF path
readRTFFile (RTFDFile path) = do
  validateFile path extensionRTFD
  readRTF $ path </> fileRTFDTXT

writeRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> Text -> m ()
writeRTFFile (RTFFileBase SRTF path) text = writeRTF path text
writeRTFFile (RTFDFile path) text = do
  validateFilePath path extensionRTFD
  let rtfPath = path </> fileRTFDTXT
  writeRTF rtfPath text

copyRTFFile :: forall m filetype. MonadIO m => RTFFile filetype -> FilePath -> m ()
copyRTFFile (RTFFile path) dstPath = liftIO $ copyFile path dstPath
copyRTFFile (RTFDFile path) dstPath = liftIO $ copyFilesRecursive path dstPath

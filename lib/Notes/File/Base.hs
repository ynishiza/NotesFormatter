module Notes.File.Base (
  listAllFilesRecursive,
  listFilesRecursive,
  copyFilesRecursive,
  isExtensionOf,
  validateFilePath,
  validateFile,
  validateFileExist,
  liftIO,
  (</>),
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (traverse_)
import System.Directory
import System.FilePath
import System.PosixCompat

validateFileExist :: MonadIO m => FilePath -> m ()
validateFileExist path = do
  exists <- liftIO $ fileExist path
  unless exists $ liftIO $ throwM $ userError $ "File not found exist:" <> path

validateFile :: MonadIO m => FilePath -> String -> m ()
validateFile path extension = do
  validateFileExist path
  validateFilePath path extension

validateFilePath :: MonadIO m => FilePath -> String -> m ()
validateFilePath path extension =
  unless (isExtensionOf extension path) $ liftIO $ throwM $ userError $ "Wrong extension:" <> path <> "\nExpected:" <> extension

listFilesRecursive :: forall m. MonadIO m => (FilePath -> Bool -> Bool) -> (FilePath -> Bool) -> FilePath -> m [FilePath]
listFilesRecursive isMatch shouldSearchDir = liftIO . makeAbsolute >=> go
 where
  go :: FilePath -> m [FilePath]
  go dirPath = do
    isDir <- liftIO $ doesDirectoryExist dirPath
    if isMatch dirPath isDir
      then return [dirPath]
      else do
        if isDir && shouldSearchDir dirPath
          then do
            content <- liftIO $ listDirectory dirPath
            join <$> traverse (go . (dirPath </>)) content
          else return []

listAllFilesRecursive :: MonadIO m => FilePath -> m [FilePath]
listAllFilesRecursive = listFilesRecursive (\_ isDir -> not isDir) (const True)

copyFilesRecursive :: MonadIO m => FilePath -> FilePath -> m ()
copyFilesRecursive srcPath dstPath = do
  allFiles <- listAllFilesRecursive srcPath
  srcAbsolute <- liftIO $ makeAbsolute srcPath
  liftIO $ traverse_ (copySingle srcAbsolute) allFiles
 where
  copySingle srcAbsolute toCopy = do
    toCopyAbsolute <- makeAbsolute toCopy
    let toCopyRelative = makeRelative srcAbsolute toCopyAbsolute
        dstFile = dstPath </> toCopyRelative
    unless (srcAbsolute </> toCopyRelative == toCopy) $ throwM $ userError $ toCopyAbsolute <> " is not in the directory tree of " <> srcAbsolute
    createDirectoryIfMissing True $ takeDirectory dstFile
    copyFileWithMetadata toCopyAbsolute dstFile

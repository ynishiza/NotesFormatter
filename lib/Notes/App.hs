module Notes.App (
  applyConfig,
  processDoc,
  processDocFromData,
  processDirAll,
  processDirRTF,
  processDirRTFD,
  processRTFFile,
  processSomeRTFFile,
  runApp,
  ProcessResult (..),
  App,
  AppOptions (..),
  mkAppOtions,
  module X,
) where

import Control.Arrow (second)
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Time
import Notes.File.RTF
import Notes.Process as X
import Notes.RTFDoc.Render
import Notes.RTFDoc.ToRTFDoc
import Notes.RTFFile
import System.Directory
import System.FilePath

type Env m = (MonadIO m, MonadLogger m, MonadReader AppOptions m, MonadBaseControl IO m, MonadCatch m)

type App = LoggingT (ReaderT AppOptions IO)

data AppOptions = AppOptions
  { appBackupDir :: FilePath
  , appTime :: ZonedTime
  , appConfig :: Config
  }

data ProcessResult = ProcessResult
  { resultMapColor :: [(ColorMap, [Int])]
  , resultMapText :: [(TextMap, Int)]
  }
  deriving stock (Eq, Show)

mkAppOtions :: FilePath -> Config -> IO AppOptions
mkAppOtions backupPath config = do
  timestamp <- getZonedTime
  return $ AppOptions backupPath timestamp config

runApp :: FilePath -> AppOptions -> App a -> IO a
runApp logPath appOptions@(AppOptions{..}) p = do
  createDirectoryIfMissing True appBackupDir
  runAppLogger LevelDebug logPath p
    & flip runReaderT appOptions

processDirAll :: Env m => FilePath -> m [(SomeRTFFile, FilePath, ProcessResult)]
processDirAll dirPath = do
  rtfdResults <- over (traverse . _1) SomeRTFFile <$> processDirRTFD dirPath
  rtfResults <- over (traverse . _1) SomeRTFFile <$> processDirRTF dirPath
  return $ rtfResults <> rtfdResults

processDirRTF :: Env m => FilePath -> m [(RTFFile 'RTF, FilePath, ProcessResult)]
processDirRTF dirPath = do
  paths <- listFilesRecursive (const . isRTF) (not . isRTFD) dirPath
  $(logInfo) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFile . rtfFile) paths

processDirRTFD :: Env m => FilePath -> m [(RTFFile 'RTFD, FilePath, ProcessResult)]
processDirRTFD dirPath = do
  paths <- listFilesRecursive (const . isRTFD) (const True) dirPath
  $(logInfo) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFile . rtfdFile) paths

processSomeRTFFile :: Env m => SomeRTFFile -> m (SomeRTFFile, FilePath, ProcessResult)
processSomeRTFFile (SomeRTFFile f) = over _1 SomeRTFFile <$> processRTFFile f

processRTFFile :: Env m => RTFFile filetype -> m (RTFFile filetype, FilePath, ProcessResult)
processRTFFile file@(AnyRTFFile srcPath) = do
  validateFileExist srcPath
  backupPath <- createBackup file

  -- step: run process
  (doc, result) <- processFile_ file
  writeRTFFile file (render doc)
  return (file, backupPath, result)

createBackup :: Env m => RTFFile filetype -> m FilePath
createBackup file@(AnyRTFFile srcPath) = do
  backupDir <- asks appBackupDir
  backupTime <- asks (formatTimestamp . appTime)
  let
    backupName =
      takeDirectory srcPath
        & splitDirectories
        & filter (/= "/")
        & intercalate "_"
        & (<> ("_" <> takeFileName srcPath))
        & ((backupTime <> "_") <>)
    backupPath = backupDir </> backupName

  -- step: backup first
  $(logInfo) $ T.pack $ "Backing up src:" <> srcPath <> "\t\t backup:" <> backupPath
  copyRTFFile file backupPath
  return backupPath

processFile_ :: forall m filetype. Env m => RTFFile filetype -> m (RTFDoc, ProcessResult)
processFile_ r@(AnyRTFFile path) = do
  content <- readRTFFile r
  processDocFromData (T.pack path) content

processDocFromData :: forall m. Env m => Text -> Text -> m (RTFDoc, ProcessResult)
processDocFromData name x = do
  case parseDoc_ (toRTFDoc @RTFDoc) x of
    Left e -> throwM $ userError e
    Right v -> processDoc name v

processDoc :: Env m => Text -> RTFDoc -> m (RTFDoc, ProcessResult)
processDoc name doc = do
  config <- asks appConfig
  let (doc', result) = applyConfig config doc
  logResult name result
  return (doc', result)

logResult :: forall m. Env m => Text -> ProcessResult -> m ()
logResult name ProcessResult{..} = do
  traverse_ logColorMap resultMapColor
  traverse_ logTextMap resultMapText
 where
  nameTag = "[" <> name <> "]"
  logColorMap :: (ColorMap, [Int]) -> m ()
  logColorMap (ColorMap{..}, num) =
    $(logInfo) $
      nameTag
        <> T.pack
          ( "[ColorMap] "
              <> show fromColor
              <> " -> "
              <> show (toColor, toColorSpace)
              <> "\t\tcount:"
              <> show num
          )
  logTextMap :: (TextMap, Int) -> m ()
  logTextMap (TextMap{..}, num) =
    $(logInfo) $
      nameTag
        <> "[TextMap] "
        <> pattern
        <> " -> "
        <> replacement
        <> "\t\tcount:"
        <> showt num

applyConfig :: Config -> RTFDoc -> (RTFDoc, ProcessResult)
applyConfig Config{..} doc =
  let
    (doc', colorResult) = mapAllColors doc
    (doc'', textResult) = mapAllTexts doc'
   in
    (doc'', ProcessResult colorResult textResult)
 where
  mapAllColors d = foldr (\c (d', res) -> second (: res) (applyColorMap c d')) (d, []) cfgColorMap
  mapAllTexts d = foldr (\c (d', res) -> second (: res) (applyTextMap c d')) (d, []) cfgTextMap

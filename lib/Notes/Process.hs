module Notes.Process (
  mapColor,
  mapPlainText,
  applyConfig,
  processDoc,
  processDocFromData,
  processDirRTF,
  processDirRTFD,
  processRTFFile,
  runApp,
  ProcessResult (..),
  App,
  AppOptions (..),
  module X,
) where

import Control.Arrow (second)
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Text qualified as T
import Notes.Config as X
import Notes.File.RTF
import Notes.RTFDoc.Render
import Notes.RTFDoc.ToRTFDoc
import Notes.RTFDoc.Types as X
import Notes.RTFFile
import System.Directory
import System.FilePath

data ProcessResult = ProcessResult
  { resultMapColor :: [(ColorMap, [Int])]
  , resultMapText :: [(TextMap, Int)]
  }
  deriving stock (Eq, Show)

type Env m = (MonadIO m, MonadLogger m, MonadReader AppOptions m, MonadBaseControl IO m, MonadCatch m)

type App = LoggingT (ReaderT AppOptions IO)

data AppOptions = AppOptions
  { appBackupDir :: FilePath
  , appConfig :: Config
  }

runApp :: FilePath -> AppOptions -> App a -> IO a
runApp logPath appOptions@(AppOptions{..}) p = do
  createDirectoryIfMissing True appBackupDir
  runAppLogger LevelDebug logPath p
    & flip runReaderT appOptions

processDirRTF :: Env m => FilePath -> m [(RTFFile 'RTF, FilePath, ProcessResult)]
processDirRTF dirPath = do
  paths <- listFilesRecursive (const . isRTF) (not . isRTFD) dirPath
  $(logInfo) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFile . rtfFile) paths

processDirRTFD :: Env m => FilePath -> m [(RTFFile 'RTFD, FilePath, ProcessResult)]
processDirRTFD dirPath = do
  paths <- listFilesRecursive (const . isRTFD) (const True) dirPath
  when (null paths) $ throwM $ userError $ "No files found in directory " <> dirPath
  $(logInfo) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFile . rtfdFile) paths

processRTFFile :: Env m => RTFFile a -> m (RTFFile a, FilePath, ProcessResult)
processRTFFile file@(RTFFile srcPath) = do
  (doc, result) <- processFile_ file
  backupDir <- asks appBackupDir
  let
    backupName =
      takeDirectory srcPath
        & splitDirectories
        & filter (/= "/")
        & intercalate "_"
        & (<> ("_" <> takeFileName srcPath))
    backupPath = backupDir </> backupName
  $(logInfo) $ T.pack $ "Backing up src:" <> srcPath <> "\t\t backup:" <> backupPath
  copyRTFFile file backupPath
  writeRTFFile file (render doc)
  return (file, backupPath, result)

processDocFromData :: forall m. Env m => Text -> Text -> m (RTFDoc, ProcessResult)
processDocFromData name x = do
  case parseDoc_ (toRTFDoc @RTFDoc) x of
    Left e -> error $ show e
    Right v -> processDoc name v

processFile_ :: forall m a. Env m => RTFFile a -> m (RTFDoc, ProcessResult)
processFile_ r@(RTFFile path) = do
  content <- readRTFFile r
  processDocFromData (T.pack path) content

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

applyColorMap :: ColorMap -> RTFDoc -> (RTFDoc, (ColorMap, [Int]))
applyColorMap c@ColorMap{..} = second (c,) . mapColor fromColor (toColor, Just toColorSpace)

applyTextMap :: TextMap -> RTFDoc -> (RTFDoc, (TextMap, Int))
applyTextMap t@TextMap{..} = second (t,) . mapPlainText pattern replacement

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor oldColor newColor doc@(RTFDoc{..}) = (doc{rtfDocHeader = rtfDocHeader{rtfColors = newColors}}, indexes)
 where
  mapToNewColor (x@(c, _) : rest) =
    let v = if c == oldColor then (True, newColor) else (False, x)
     in v : mapToNewColor rest
  mapToNewColor [] = []
  colorResult = mapToNewColor $ rtfColors rtfDocHeader
  indexes =
    fst <$> colorResult
      & zip [0 :: Int ..]
      & filter ((== True) . snd)
      <&> fst
  newColors = snd <$> colorResult

mapPlainText :: Text -> Text -> RTFDoc -> (RTFDoc, Int)
mapPlainText pattern replacement doc@(RTFDoc{..}) = (doc{rtfDocContent = newContent}, count)
 where
  (count, newContent) = mapContent (0, rtfDocContent)
  mapContent :: (Int, [RTFContent]) -> (Int, [RTFContent])
  mapContent (i :: Int, RTFText text : rest) =
    let
      (i', text') = if T.isInfixOf pattern text then (i + 1, T.replace pattern replacement text) else (i, text)
      (i'', rest') = mapContent (i', rest)
     in
      (i'', RTFText text' : rest')
  mapContent (i, x : rest) = second (x :) $ mapContent (i, rest)
  mapContent (i, []) = (i, [])

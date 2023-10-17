{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

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
  resultTable,
  emptyAppOptions,
  module X,
) where

import Colonnade
import Control.Arrow (second)
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.List.Extra (trim)
import Data.List.NonEmpty qualified as N
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Time
import Notes.File.RTF
import Notes.Process as X
import Notes.RTFDoc.Render
import Notes.RTFDoc.ToRTFDoc
import Notes.RTFFile
import System.Directory
import System.FilePath
import Text.Regex.TDFA

type Env m = (MonadIO m, MonadLogger m, MonadReader AppOptions m, MonadBaseControl IO m, MonadCatch m)

type App = LoggingT (ReaderT AppOptions IO)

data AppOptions = AppOptions
  { appLogLevel :: LogLevel
  , appBackupDir :: FilePath
  , appPattern :: Maybe String
  , appInteractive :: Bool
  , appTime :: ZonedTime
  , appConfig :: Config
  }

data ProcessResult = ProcessResult
  { resultMapColor :: [(ColorMap, [Int])]
  , resultMapText :: [(TextMap, Int)]
  , resultMapContent :: [(ContentMap, Int)]
  , resultMapFont :: [(FontMap, [Int])]
  }
  deriving stock (Eq, Show)

emptyAppOptions :: IO AppOptions
emptyAppOptions = do
  timestamp <- getZonedTime
  return $
    AppOptions
      { appLogLevel = LevelInfo
      , appBackupDir = ""
      , appPattern = Nothing
      , appInteractive = False
      , appTime = timestamp
      , appConfig = emptyConfig
      }

runApp :: FilePath -> AppOptions -> App a -> IO a
runApp logPath appOptions@(AppOptions{..}) p = do
  createDirectoryIfMissing True appBackupDir
  runAppLogger appLogLevel logPath p
    & flip runReaderT appOptions

processDirAll :: Env m => FilePath -> m [(SomeRTFFile, FilePath, ProcessResult)]
processDirAll dirPath = do
  rtfdResults <- over (traverse . _1) SomeRTFFile <$> processDirRTFD dirPath
  rtfResults <- over (traverse . _1) SomeRTFFile <$> processDirRTF dirPath
  return $ rtfResults <> rtfdResults

processDirRTF :: Env m => FilePath -> m [(RTFFile 'RTF, FilePath, ProcessResult)]
processDirRTF dirPath = do
  paths <- listFilesRecursive (const . isRTF) (not . isRTFD) dirPath
  $(logDebug) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFileMaybe . rtfFile) paths
    <&> catMaybes

processDirRTFD :: Env m => FilePath -> m [(RTFFile 'RTFD, FilePath, ProcessResult)]
processDirRTFD dirPath = do
  paths <- listFilesRecursive (const . isRTFD) (const True) dirPath
  $(logDebug) $ T.pack $ "Files: " <> intercalate "," paths
  traverse (processRTFFileMaybe . rtfdFile) paths
    <&> catMaybes

processSomeRTFFile :: Env m => SomeRTFFile -> m (SomeRTFFile, FilePath, ProcessResult)
processSomeRTFFile (SomeRTFFile f) = over _1 SomeRTFFile <$> processRTFFile f

processRTFFileMaybe :: Env m => RTFFile filetype -> m (Maybe (RTFFile filetype, FilePath, ProcessResult))
processRTFFileMaybe file@(AnyRTFFile srcPath) = do
  isInteractive <- asks appInteractive
  pattern <- asks appPattern
  let run = Just <$> processRTFFile file
  runPattern pattern $ if isInteractive then runInteractive run else run
 where
  runPattern (Just (pattern :: String)) action = do
    if rtfFilePath file =~ pattern then action else skip
  runPattern Nothing action = action
  runInteractive action = do
    res <- liftIO $ putStrLn ("Process " <> srcPath <> "? (y/n)") >> getLine
    if res /= trim "n" then action else skip
  skip = do
    let message = "Skip " <> srcPath
    $(logDebug) (T.pack message)
    return Nothing

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
  (doc', result) <- applyConfig config doc
  logResult name result
  return (doc', result)

logResult :: forall m. Env m => Text -> ProcessResult -> m ()
logResult name ProcessResult{..} = do
  traverse_ logColorMap resultMapColor
  traverse_ logTextMap resultMapText
  traverse_ logFontMap resultMapFont
 where
  nameTag = "[" <> name <> "]"
  logColorMap :: (ColorMap, [Int]) -> m ()
  logColorMap (ColorMap{..}, num) = logCounts "ColorMap" (show fromColor) (show (toColor, toColorSpace)) num
  logTextMap :: (TextMap, Int) -> m ()
  logTextMap (TextMap{..}, num) = logCounts "TextMap" (T.unpack pattern) (T.unpack replacement) num
  logFontMap (FontMap{..}, num) = logCounts "FontMap" (T.unpack fromFontName) (T.unpack $ fmFontName toFont) num
  logCounts :: Show a => String -> String -> String -> a -> m ()
  logCounts configName fromValue toProperty count =
    $(logDebug) $
      nameTag
        <> T.pack
          ( "["
              <> configName
              <> "] "
              <> fromValue
              <> " -> "
              <> toProperty
              <> "\t\tcount:"
              <> show count
          )

applyConfig :: MonadCatch m => Config -> RTFDoc -> m (RTFDoc, ProcessResult)
applyConfig Config{..} doc =
  let
    res :: Either ProcessError (RTFDoc, ProcessResult)
    res = do
      (doc', colorResult) <- pure $ mapAllColors doc
      (doc'', textResult) <- pure $ mapAllTexts doc'
      (doc''', contentResult) <- pure $ mapAllContents doc''
      (doc'''', fontResult) <- pure $ mapAllFonts doc'''
      return
        ( doc''''
        , ProcessResult
            { resultMapColor = colorResult
            , resultMapText = textResult
            , resultMapContent = contentResult
            , resultMapFont = fontResult
            }
        )
   in
    case res of
      Right v -> return v
      Left e -> throwM e
 where
  mapAllColors d = foldr (\cfg (d', result) -> second (: result) (applyColorMap cfg d')) (d, []) cfgColorMap
  mapAllTexts d = foldr (\cfg (d', result) -> second (: result) (applyTextMap cfg d')) (d, []) cfgTextMap
  mapAllContents d = foldr (\cfg (d', result) -> second (: result) (applyContentMap cfg d')) (d, []) cfgContentMap
  mapAllFonts d = foldr (\cfg (d', result) -> second (: result) (applyFontMap cfg d')) (d, []) cfgFontMap

resultTable :: [(SomeRTFFile, FilePath, ProcessResult)] -> String
resultTable = ascii processResultColumns

processResultColumns :: Colonnade Headed (SomeRTFFile, FilePath, ProcessResult) String
processResultColumns =
  mconcat
    [ headed "Source RTF" (views _1 (withSomeRTFFile rtfFilePath))
    , headed "Backup" (views _2 id)
    , headed "ColorMap" (views _3 columnTextColorMap)
    , headed "ContentMap" (views _3 columnContentMap)
    , headed "TextMap" (views _3 columnTextTextMap)
    , headed "FontMap" (views _3 columnTextFontMap)
    ]

columnTextMany :: (a -> Maybe String) -> [a] -> String
columnTextMany f list =
  list
    <&> f
    & catMaybes
    & intercalate ":"

columnTextColorMap :: ProcessResult -> String
columnTextColorMap (ProcessResult{resultMapColor}) = columnTextMany single resultMapColor
 where
  single (ColorMap{..}, indexes@(not . null -> True)) = Just $ T.unpack $ render fromColor <> " -> " <> render toColor <> " " <> showt (length indexes)
  single _ = Nothing

columnTextTextMap :: ProcessResult -> String
columnTextTextMap (ProcessResult{resultMapText}) = columnTextMany single resultMapText
 where
  single (TextMap{..}, count@((> 0) -> True)) = Just $ T.unpack $ pattern <> " -> " <> replacement <> " " <> showt count
  single _ = Nothing

columnContentMap :: ProcessResult -> String
columnContentMap (ProcessResult{resultMapContent}) = columnTextMany single resultMapContent
 where
  single (ContentMap{..}, count@((> 0) -> True)) = Just $ T.unpack $ render (N.toList fromContents) <> " -> " <> render (N.toList toContents) <> " " <> showt count
  single _ = Nothing

columnTextFontMap :: ProcessResult -> String
columnTextFontMap (ProcessResult{resultMapFont}) = columnTextMany single resultMapFont
 where
  single (FontMap{..}, indexes@(not . null -> True)) = Just $ T.unpack $ fromFontName <> " -> " <> fmFontName toFont <> " " <> showt (length indexes)
  single _ = Nothing

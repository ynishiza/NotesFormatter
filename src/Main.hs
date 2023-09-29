{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Exception.Safe (throwM)
import Control.Monad
import Data.Aeson (eitherDecodeFileStrict')
import Notes.App
import Notes.RTFFile
import Options.Applicative

main :: IO ()
main = do
  Options{..} <- execParser parseOpts
  when (interactive && not recursive) $ throwM $ userError "--interactive is only supported for --recursive"
  eitherDecodeFileStrict' @Config configPath >>= \case
    Left message -> throwM $ userError $ "Error parsing config " <> configPath <> "\n" <> message
    Right config -> do
      appConfig <-
        emptyAppOptions
          <&> \options ->
            options
              { appConfig = config
              , appPattern = pattern
              , appInteractive = interactive
              , appBackupDir = backupPath
              }
      let timestamp = formatTimestamp $ appTime appConfig
          logPath = timestamp <> "_" <> "rtf.log"

      if recursive
        then do
          res <- runApp logPath appConfig (processDirAll rtfPath)
          let msg = "Processed " <> show (length res) <> " files"
          appendFile logPath $ show res
          appendFile logPath msg
          putStrLn msg
          return ()
        else do
          res <- runApp logPath appConfig (processSomeRTFFile $ rtfFromPath rtfPath)
          let msg = "Processed " <> rtfPath
          appendFile logPath $ show res
          appendFile logPath msg
          putStrLn msg
          return ()

data Options = Options
  { configPath :: FilePath
  , recursive :: Bool
  , interactive :: Bool
  , pattern :: Maybe String
  , backupPath :: FilePath
  , rtfPath :: FilePath
  }

parseOpts :: ParserInfo Options
parseOpts = info (helper <*> parseSimple) (fullDesc <> progDesc "")

parseSimple :: Parser Options
parseSimple =
  Options
    <$> strOption (long "config" <> short 'c' <> metavar "CONFIGPATH" <> help "Path to configuration")
    <*> switch (long "recursive" <> short 'r' <> help "Process directory")
    <*> switch (long "interactive" <> short 'i' <> help "Ask before processing each file")
    <*> optional (strOption (long "regex" <> short 'g' <> metavar "PATTERN" <> help ""))
    <*> strOption (long "backupdir" <> short 'b' <> metavar "PATH" <> value "rtfbak" <> help "Backup directory")
    <*> strArgument (metavar "SRCPATH")

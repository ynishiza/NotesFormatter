{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Exception.Safe (throwM)
import Control.Monad
import Data.Aeson (eitherDecodeFileStrict')
import Data.Maybe (isJust)
import Notes.App
import Notes.RTFFile
import Options.Applicative

main :: IO ()
main = do
  cliOpts@CLIOptions{..} <- execParser parseOpts
  when debugMode $ putStrLn $ "Options:" <> show cliOpts
  unless recursive $ do
    when (isJust pattern) $ throwM $ userError "--pattern is only supported for --recursive"

  eitherDecodeFileStrict' @Config configPath >>= \case
    Left message -> throwM $ userError $ "Error parsing config " <> configPath <> "\n" <> message
    Right config -> do
      appConfig <-
        emptyAppOptions
          <&> \options ->
            options
              { appConfig = config
              , appLogLevel = if debugMode then LevelDebug else LevelInfo
              , appPattern = pattern
              , appInteractive = interactive
              , appBackupDir = backupPath
              }

      let timestamp = formatTimestamp $ appTime appConfig
          logPath = timestamp <> "_" <> "rtf.log"

      if recursive
        then do
          res <- runApp logPath appConfig (processDirAll rtfPath)
          reportResult logPath res
        else do
          res <- runApp logPath appConfig (processSomeRTFFile $ rtfFromPath rtfPath)
          reportResult logPath [res]
 where
  reportResult logPath result = do
    let msg = "Processed files:" <> show (length result)
        table = resultTable result

    appendFile logPath $ show result
    appendFile logPath $ unlines [table, msg]
    putStrLn $ unlines [table, msg, "Log in " <> logPath]

data CLIOptions = CLIOptions
  { debugMode :: Bool
  , configPath :: FilePath
  , recursive :: Bool
  , interactive :: Bool
  , pattern :: Maybe String
  , backupPath :: FilePath
  , rtfPath :: FilePath
  }
  deriving (Show, Eq)

parseOpts :: ParserInfo CLIOptions
parseOpts = info (helper <*> parseSimple) (fullDesc <> progDesc "")

parseSimple :: Parser CLIOptions
parseSimple =
  CLIOptions
    <$> switch (long "debug" <> short 'd')
    <*> strOption (long "config" <> short 'c' <> metavar "CONFIGPATH" <> help "Path to configuration")
    <*> switch (long "recursive" <> short 'r' <> help "Process directory")
    <*> switch (long "interactive" <> short 'i' <> help "Ask before processing each file")
    <*> optional (strOption (long "regex" <> short 'e' <> metavar "PATTERN" <> help "Extended regular expression"))
    <*> strOption (long "backupdir" <> short 'b' <> metavar "PATH" <> value "rtfbak" <> help "Backup directory")
    <*> strArgument (metavar "SRCPATH")

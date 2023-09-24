-- import B.Lib qualified
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- import Data.Text.IO qualified as T

-- import Notes.Process
import Notes.App

-- import Notes.RTFDoc hiding (Parser)
import Options.Applicative

-- import System.IO (IOMode (..), withFile)
import Notes.RTFFile

main :: IO ()
main = do
  Options{..} <- execParser parseOpts
  (Just config) <- decodeFileStrict' @Config configPath
  appConfig <- mkAppOtions backupPath config
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
    <*> strOption (long "backupdir" <> short 'b' <> metavar "PATH" <> value "rtfbak" <> help "Backup directory")
    <*> strArgument (metavar "SRCPATH")

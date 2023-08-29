-- import B.Lib qualified
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

import Data.Text.IO qualified as T
import Notes.Process
import Options.Applicative
import Notes.RTFDoc.ToRTFDoc 
import System.IO (withFile, IOMode (..))
import Notes.RTFDoc hiding (Parser)

main :: IO ()
main = do
  Options {..} <- execParser parseOpts
  (Just config) <- decodeFileStrict' @Config configPath
  d <- withFile rtfPath ReadMode $ \h -> do
    x <- T.hGetContents h
    case parseDoc_ (toRTFDoc @RTFDoc) x of
      Left e -> error $ show e
      Right v -> let (d, _, _) = applyConfig config v in return d

  T.writeFile "temp.rtf" $  render d
  T.putStrLn "Done"

data Options = Options
  { 
    configPath :: FilePath,
    rtfPath :: FilePath
  }

parseOpts :: ParserInfo Options
parseOpts = info (helper <*> subparser cmd) (fullDesc <> progDesc "")

cmd :: Mod CommandFields Options
cmd = command "simple" (info (helper <*> parseSimple ) (fullDesc <> progDesc ""))

parseSimple :: Parser Options
parseSimple =
  Options
    <$> strOption (short 'c' <> metavar "CONFIGPATH") 
    <*> strArgument (metavar "RTFPATH" <> help "")

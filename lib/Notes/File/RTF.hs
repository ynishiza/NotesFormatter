module Notes.File.RTF (
  extensionRTF,
  extensionRTFD,
  fileRTFDTXT,
  readRTF,
  readRTFD,
  writeRTFD,
  writeRTF,
  isRTF,
  isRTFD,
  isRTFDTXT,
  module X,
) where

import Control.Monad.Catch
import Control.Monad.Trans
import Data.ByteString qualified as B
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Notes.File.Base as X
import GHC.IO (evaluate)
import System.FilePath

isRTF :: FilePath -> Bool
isRTF = isExtensionOf extensionRTF

isRTFD :: FilePath -> Bool
isRTFD = isExtensionOf extensionRTFD

isRTFDTXT :: FilePath -> Bool
isRTFDTXT path = isRTFD (takeDirectory path) && isRTF (takeFileName path)

fileRTFDTXT :: IsString s => s
fileRTFDTXT = "TXT.rtf"

extensionRTF :: IsString s => s
extensionRTF = "rtf"

extensionRTFD :: IsString s => s
extensionRTFD = "rtfd"

readRTF :: FilePath -> IO Text
readRTF path = do
  validateFile path extensionRTF
  B.readFile path >>= decodeRTFFile

readRTFD :: FilePath -> IO Text
readRTFD path = do
  validateFile path extensionRTFD
  let rtfPath = path </> fileRTFDTXT
  validateFile rtfPath extensionRTF
  B.readFile rtfPath >>= decodeRTFFile

writeRTF :: MonadIO m => FilePath -> Text -> m ()
writeRTF path text = do
  validateFilePath path extensionRTF
  liftIO $ B.writeFile path $ encodeUtf8 text

writeRTFD :: MonadIO m => FilePath -> Text -> m ()
writeRTFD path text = do
  validateFilePath path extensionRTFD
  let rtfPath = path </> fileRTFDTXT
  writeRTF rtfPath text

decodeRTFFile :: forall m. MonadIO m => B.ByteString -> m Text
decodeRTFFile dat = liftIO $ catch asUTF8 $ \case
  (DecodeError _ _) -> asLatin1
  e -> throwM e
 where
  asUTF8 = evaluate $ decodeUtf8 dat
  asLatin1 = evaluate $ decodeLatin1 dat


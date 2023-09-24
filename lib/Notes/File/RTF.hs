module Notes.File.RTF (
  extensionRTF,
  extensionRTFD,
  fileRTFDTXT,
  isRTF,
  isRTFD,
  isRTFDTXT,
  readRTF,
  writeRTF,
  module X,
) where

import Control.Exception.Safe
import Control.Monad.Trans
import Data.ByteString qualified as B
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import GHC.IO (evaluate)
import Notes.File.Base as X
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

readRTF :: MonadIO m => FilePath -> m Text
readRTF path = do
  validateFile path extensionRTF
  liftIO $ B.readFile path >>= decodeRTFFile

writeRTF :: MonadIO m => FilePath -> Text -> m ()
writeRTF path text = do
  validateFilePath path extensionRTF
  liftIO $ B.writeFile path $ encodeUtf8 text

decodeRTFFile :: forall m. MonadIO m => B.ByteString -> m Text
decodeRTFFile dat = liftIO $ catch asUTF8 $ \case
  (DecodeError _ _) -> asLatin1
  e -> throwM e
 where
  asUTF8 = evaluate $ decodeUtf8 dat
  asLatin1 = evaluate $ decodeLatin1 dat

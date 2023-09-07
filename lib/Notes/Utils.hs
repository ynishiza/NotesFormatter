{-# LANGUAGE UndecidableInstances #-}

module Notes.Utils (
  module X,
  Text,
  TextShow (..),
  basePath,
  dataLensRules,
  makeLensesWith,
  makePrisms,
  Generic,
  Proxy (..),
  defaultShowt,
  (</>),
  runAppLogger,
) where

import Control.Lens hiding (from, to)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.ByteString qualified as B
import Data.Function as X
import Data.Functor as X
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO
import TextShow (TextShow (..))

basePath :: FilePath
basePath =
  $( location
      >>= runIO . makeAbsolute . loc_filename
      <&> LitE . StringL . takeDirectory . takeDirectory . takeDirectory
   )

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s]

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer

runAppLogger :: forall m a. MonadBaseControl IO m => LogLevel -> FilePath -> LoggingT m a -> m a
runAppLogger minLevel logPath app = liftBaseOp (withFile logPath WriteMode) runApp
 where
  runApp handle = runLoggingT app $ \loc src level message -> do
    let message' = fromLogStr $ defaultLogStr loc src level message
    when (level >= minLevel) $ liftBase $ B.putStr message'
    liftBase $ B.hPutStr handle message'

defaultShowt :: Show a => a -> Text
defaultShowt = T.pack . show

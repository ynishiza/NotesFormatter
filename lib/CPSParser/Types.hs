module CPSParser.Types (
  CParserT (..),
  CParserError (..),
  module X,
  execCParser,
) where

import Control.Applicative as X
import Control.Arrow (first)
import Control.Exception (Exception)
import Control.Monad as X
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

data CParserError state = HasCallStack => CParserError state Text

deriving stock instance Show state => Show (CParserError state)
deriving anyclass instance (Show state, Typeable state) => Exception (CParserError state)

type OnSuccess state result m a = (a, state) -> m result

type OnFail error result m = error -> m result

-- | CPS style parser
newtype CParserT error state result m a = CParserT
  { runCParserBase :: state -> OnSuccess state result m a -> OnFail error result m -> m result
  }

instance Functor (CParserT error state result m) where
  fmap f (CParserT p) = CParserT $ \state k ke -> p state (k . first f) ke

instance Applicative (CParserT error state result m) where
  pure x = CParserT $ \state k _ -> k (x, state)
  CParserT fp <*> CParserT xp = CParserT $ \state k ke -> fp state (\(f, state') -> xp state' (k . first f) ke) ke

instance Alternative (CParserT (CParserError state) state result m) where
  empty = CParserT $ \state _ ke -> ke (CParserError state "empty")
  CParserT xp <|> CParserT yp = CParserT $ \state k ke -> xp state k (\_ -> yp state k ke)

instance MonadPlus (CParserT (CParserError state) state result m)

instance Monad (CParserT error state result m) where
  CParserT xp >>= h = CParserT $ \state k ke -> xp state (\(x, state') -> (runCParserBase $ h x) state' k ke) ke

instance MonadFail (CParserT (CParserError state) state result m) where
  fail msg = CParserT $ \state _ ke -> ke (CParserError state (T.pack msg))

execCParser :: Monad m => CParserT error state (Either error (a, state)) m a -> state -> m (Either error (a, state))
execCParser (CParserT f) i = f i (return . Right) (return . Left)

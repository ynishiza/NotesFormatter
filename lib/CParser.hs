module CParser (
  CParserT (..),
  CParserError (..),
  module X,
  execCParser,
) where

import Control.Applicative as X
import Control.Arrow (first)
import Control.Exception (Exception)
import Control.Monad as X
import Data.Attoparsec.Combinator as X
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

data CParserError state = HasCallStack => CParserError state Text

deriving stock instance Show state => Show (CParserError state)
deriving anyclass instance (Show state, Typeable state) => Exception (CParserError state)

-- CPS parser
newtype CParserT error state result m a = CParserT {runCParserBase :: state -> ((a, state) -> m result) -> (error -> m result) -> m result}

instance Functor (CParserT error state result m) where
  fmap f (CParserT p) = CParserT $ \content k ke -> p content (k . first f) ke

instance Applicative (CParserT error state result m) where
  pure x = CParserT $ \content k _ -> k (x, content)
  CParserT fp <*> CParserT xp = CParserT $ \content k ke -> fp content (\(f, content') -> xp content' (k . first f) ke) ke

instance Alternative (CParserT (CParserError state) state result m) where
  empty = CParserT $ \content _ ke -> ke (CParserError content "empty")
  CParserT xp <|> CParserT yp = CParserT $ \content k ke -> xp content k (\_ -> yp content k ke)

instance MonadPlus (CParserT (CParserError state) state result m)

instance Monad (CParserT error state result m) where
  CParserT xp >>= h = CParserT $ \content k ke -> xp content (\(x, content') -> (runCParserBase $ h x) content' k ke) ke

instance MonadFail (CParserT (CParserError state) state result m) where
  fail msg = CParserT $ \state _ ke -> ke (CParserError state (T.pack msg))

execCParser :: Monad m => CParserT error state (Either error (a, state)) m a -> state -> m (Either error (a, state))
execCParser (CParserT f) i = f i (return . Right) (return . Left)

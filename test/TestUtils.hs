module TestUtils (
  multiline
  ) where

import Language.Haskell.TH.Quote
import Data.Text.Encoding
import Data.Text qualified as T

multiline :: QuasiQuoter
multiline = QuasiQuoter {
  quoteExp = \s ->  [| encodeUtf8 $ T.pack s |]
            }

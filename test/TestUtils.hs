module TestUtils (
  multiline,
  rtfPath,
  ) where

import Language.Haskell.TH.Quote
import Data.Text.Encoding
import Data.Text qualified as T
import Notes.Utils

rtfPath :: FilePath
rtfPath = basePath </> "data" </> "rtf"

multiline :: QuasiQuoter
multiline = QuasiQuoter {
  quoteExp = \s ->  [| encodeUtf8 $ T.pack s |],
  quoteDec = undefined,
  quoteType = undefined,
  quotePat = undefined
            }

module TestUtils (
  multiline,
  rtfPath,
) where

import Data.Text qualified as T
import Language.Haskell.TH.Quote
import Notes.Utils

rtfPath :: FilePath
rtfPath = basePath </> "data" </> "rtf"

multiline :: QuasiQuoter
multiline =
  QuasiQuoter
    { quoteExp = \s -> [|T.pack s|]
    , quoteDec = undefined
    , quoteType = undefined
    , quotePat = undefined
    }

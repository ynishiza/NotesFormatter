module Utils (
  module X,
  Text,
  TextShow (..),
  basePath,
  dataLensRules,
  makeLensesWith,
  makePrisms,
) where

import Control.Lens.TH
import Control.Lens
import Data.Function as X
import Data.Functor as X
import Data.Text (Text)
import Language.Haskell.TH
import System.Directory
import System.FilePath
import TextShow (TextShow (..))

basePath :: FilePath
basePath =
  $( location
      >>= runIO . makeAbsolute . loc_filename
      <&> LitE . StringL . takeDirectory
   )

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s] 

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer

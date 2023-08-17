module Utils (
  module X,
  Text,
  TextShow (..),
  basePath,
  dataLensRules,
  makeLensesWith,
  makePrisms,
  Generic,
  Proxy (..),
  ConstructorInfo (..),
  getTypeName,
  getNameBase,
) where

import Control.Lens hiding (from, to)
import Data.Function as X
import Data.Functor as X
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics
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

class ConstructorInfo c where
  cname :: c a -> Text

instance ConstructorInfo f => ConstructorInfo (M1 D c f) where
  cname (M1 x) = cname x

instance (ConstructorInfo a, ConstructorInfo b) => ConstructorInfo (a :+: b) where
  cname (L1 x) = cname x
  cname (R1 x) = cname x

instance Constructor c => ConstructorInfo (M1 C c f) where
  cname x = T.pack $ conName x

getNameBase :: Name -> Q Exp
getNameBase name = do
  (TyConI t) <- reify name
  case t of
    (DataD _ n _ _ _ _) -> return $ LitE $ StringL $ nameBase n
    (NewtypeD _ n _ _ _ _) -> return $ LitE $ StringL $ nameBase n
    (TySynD n _ _) -> return $ LitE $ StringL $ nameBase n
    _ -> undefined

getTypeName :: forall c. Proxy c -> Q Exp
getTypeName _ = getNameBase ''c

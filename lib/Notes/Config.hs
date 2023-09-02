module Notes.Config (
  Config (..),
  ColorMap (..),
  TextMap (..),
  --
  _Config,
  _cfgColorMap,
  _cfgTextMap,
  decodeFileStrict',
  eitherDecode',
  FromJSON (..),
) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Scientific
import Data.Vector
import Notes.RTFDoc hiding (Parser)

data Config = Config
  { cfgColorMap :: [ColorMap]
  , cfgTextMap :: [TextMap]
  }
  deriving stock (Show, Eq, Generic)

data ColorMap = ColorMap
  { fromColor :: RTFColor
  , toColor :: RTFColor
  , toColorSpace :: ColorSpace
  }
  deriving stock (Show, Eq, Generic)

data TextMap = TextMap
  { pattern :: Text
  , replacement :: Text
  }
  deriving stock (Show, Eq, Generic)

$(makeLensesWith dataLensRules ''Config)
$(makePrisms ''Config)

instance FromJSON Config where
  parseJSON = genericParseJSON opts
   where
    opts =
      defaultOptions
        { fieldLabelModifier = \case
            ('c' : 'f' : 'g' : c : rest) -> toLower c : rest
            _ -> undefined
        }

instance FromJSON ColorMap where
  parseJSON = withObject "ColorMap" $ \obj -> do
    fromColor <- obj .: "from" >>= colorObj
    toColors <- obj .: "to"
    ColorMap fromColor
      <$> colorObj toColors
      <*> colorspaceObj toColors
   where
    colorspaceObj = withObject "ColorSpace obj" $ \obj -> obj .: "colorSpace" >>= colorspace
    colorObj = withObject "RTFColor obj" $ \obj -> obj .: "color" >>= rtfColor
    colorspace = withObject "" $ \obj ->
      (obj .: "csgray" >>= csgray)
        <|> (obj .: "cssrgb" >>= cssrgb)
        <|> (obj .: "csgenericrgb" >>= csgeneric)
    rtfColor = parseList "RTFColor" $ \case
      [r, g, b] ->
        Just $
          RTFColor
            <$> optional (parseIntegral "red" r)
            <*> optional (parseIntegral "green" g)
            <*> optional (parseIntegral "blue" b)
      _ -> Nothing
    csgray v = CSGray <$> parseIntegral "CSGray" v
    cssrgb = parseList "CSSRGB" $ \case
      [r, g, b] ->
        Just $ CSSRGB <$> parseIntegral "c" r <*> parseIntegral "c" g <*> parseIntegral "c" b
      _ -> Nothing
    csgeneric = parseList "CSGenericRGB" $ \case
      [r, g, b] ->
        Just $ CSGenericRGB <$> parseIntegral "c" r <*> parseIntegral "c" g <*> parseIntegral "c" b
      _ -> Nothing

instance FromJSON TextMap

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral name = withScientific name $ \v -> case floatingOrInteger @Double v of
  Right i -> return i
  Left x -> fail $ "[" <> name <> "] not an integer" <> show x

parseList :: String -> ([Value] -> Maybe (Parser a)) -> Value -> Parser a
parseList name f = withArray name $ \arr -> case f (toList arr) of
  Just p -> p
  Nothing -> fail $ "Failed to parse array for " <> name

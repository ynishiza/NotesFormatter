{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Notes.Config (
  Config (..),
  ColorMap (..),
  TextMap (..),
  ContentMap (..),
  FontMap (..),
  FontMapFont (..),
  --
  _Config,
  _cfgColorMap,
  _cfgTextMap,
  _cfgFontMap,
  decodeFileStrict',
  eitherDecode',
  FromJSON (..),
  emptyConfig,
  -- Lens
  _fromColor,
  _toColor,
  _toColorSpace,
  _pattern,
  _replacement,
  _fromContents,
  _toContents,
  _fromFontName,
  _toFont,
) where

import Control.Lens
import Control.Monad.Combinators (many)
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Exts (IsString)
import Notes.RTFDoc hiding (Parser)
import Text.Megaparsec (eof)

data Config = Config
  { cfgColorMap :: [ColorMap]
  , cfgTextMap :: [TextMap]
  , cfgFontMap :: [FontMap]
  }
  deriving stock (Show, Eq, Generic)

emptyConfig :: Config
emptyConfig = Config [] [] []

data ColorMap = ColorMap
  { fromColor :: RTFColor
  , toColor :: RTFColor
  , toColorSpace :: ColorSpace
  }
  deriving stock (Show, Eq, Generic)

-- TODO: replace symbol
-- e.g.  \'85 = …
data TextMap = TextMap
  { pattern :: Text
  , replacement :: Text
  }
  deriving stock (Show, Eq, Generic)

data ContentMap = ContentMap
  { fromContents :: NonEmpty RTFDocContent
  , toContents :: NonEmpty RTFDocContent
  }
  deriving stock (Show, Eq, Generic)

data FontMapFont = FontMapFont
  { fmFamily :: FontFamily
  , fmFontName :: Text
  }
  deriving stock (Show, Eq, Generic)

data FontMap = FontMap
  { fromFontName :: Text
  , toFont :: FontMapFont
  }
  deriving stock (Show, Eq, Generic)

$(makePrisms ''Config)
$(makeLensesWith dataLensRules ''Config)
$(makeLensesWith dataLensRules ''TextMap)
$(makeLensesWith dataLensRules ''ColorMap)
$(makeLensesWith dataLensRules ''FontMapFont)
$(makeLensesWith dataLensRules ''FontMap)
$(makeLensesWith dataLensRules ''ContentMap)

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
    fromColor <- (obj .: "from") >>= inPath (Key "from") colorObj
    toObj <- obj .: "to"
    ( ColorMap fromColor
        <$> colorObj toObj
        <*> colorspaceObj toObj
      )
      <?> Key "to"
   where
    colorspaceObj = withObject "ColorSpace obj" (\obj -> obj .: "colorSpace" >>= inPath (Key "colorSpace") colorspace)
    colorObj = withObject "RTFColor obj" (\obj -> obj .: "color" >>= inPath (Key "color") rtfColor)
    colorspace =
      withObject
        "colorSpace"
        ( \obj -> do
            vgray <- obj .:? "csgray"
            vsrgb <- obj .:? "cssrgb"
            vgeneric <- obj .:? "csgenericrgb"
            case vgray of
              Just x -> csgray x <?> Key "csgray"
              Nothing -> case vsrgb of
                Just x -> cssrgb x <?> Key "cssrgb"
                Nothing -> case vgeneric of
                  Just x -> csgeneric x <?> Key "csgenericrgb"
                  Nothing -> fail "colorspace"
        )
    rtfColor =
      parseList
        "RTFColor"
        ( \case
            [r, g, b] ->
              Just $
                RTFColor
                  <$> (nullOrIntegral (parseIntegral "red") r <?> Index 0)
                  <*> (nullOrIntegral (parseIntegral "green") g <?> Index 1)
                  <*> (nullOrIntegral (parseIntegral "blue") b <?> Index 2)
            _ -> Nothing
        )
    csgray v = CSGray <$> (parseIntegral "CSGray" v <?> Index 0)
    cssrgb =
      parseList
        "CSSRGB"
        ( \case
            [r, g, b] ->
              Just $
                CSSRGB
                  <$> (parseIntegral "c" r <?> Index 0)
                  <*> (parseIntegral "c" g <?> Index 1)
                  <*> (parseIntegral "c" b <?> Index 2)
                  <*> pure Nothing
            _ -> Nothing
        )
    csgeneric =
      parseList
        "CSGenericRGB"
        ( \case
            [r, g, b] ->
              Just $
                CSGenericRGB
                  <$> (parseIntegral "c" r <?> Index 0)
                  <*> (parseIntegral "c" g <?> Index 1)
                  <*> (parseIntegral "c" b <?> Index 2)
                  <*> pure Nothing
            _ -> Nothing
        )
    nullOrIntegral :: (Value -> Parser a) -> Value -> Parser (Maybe a)
    nullOrIntegral _ Null = return Nothing
    nullOrIntegral p v = Just <$> p v

instance FromJSON TextMap

instance FromJSON ContentMap where
  parseJSON = withObject "" $ \obj -> do
    ContentMap
      <$> (((obj .: "fromContents") >>= f) <?> Key "fromContents")
      <*> (((obj .: "toContents") >>= f) <?> Key "toContents")
   where
    f :: Value -> Parser (NonEmpty RTFDocContent)
    f = withText "" $ \text -> case parseDoc_ (many (toRTFDoc @RTFDocContent) <* eof) text of
      Right (v : rest) -> return $ v :| rest
      Right [] -> fail "Empty"
      Left e -> fail e

instance FromJSON FontFamily where
  parseJSON = withText "Font family" $ \str -> case inverseMap showFontMapFamily str of
    Just v -> return v
    Nothing -> fail $ "Unknown font family " <> T.unpack str <> ". Should be one of " <> intercalate "," (show <$> allFontFamilies)

showFontMapFamily :: IsString s => FontFamily -> s
showFontMapFamily FNil = "nil"
showFontMapFamily FRoman = "roman"
showFontMapFamily FSwiss = "swiss"
showFontMapFamily FModern = "modern"
showFontMapFamily FScript = "script"
showFontMapFamily FDecor = "decor"
showFontMapFamily FTech = "tech"
showFontMapFamily FBidi = "bidi"

instance FromJSON FontMapFont where
  parseJSON = genericParseJSON options
   where
    options = defaultOptions{fieldLabelModifier = unCap . drop 2, rejectUnknownFields = True}
    unCap "" = ""
    unCap (a : as) = toLower a : as

instance FromJSON FontMap

inPath :: JSONPathElement -> (Value -> Parser a) -> Value -> Parser a
inPath path p = (<?> path) . p

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral name = withScientific name $ \v -> case floatingOrInteger @Double v of
  Right i -> return i
  Left x -> fail $ "[" <> name <> "] not an integer" <> show x

parseList :: String -> ([Value] -> Maybe (Parser a)) -> Value -> Parser a
parseList name f = withArray name $ \arr -> case f (V.toList arr) of
  Just p -> p
  Nothing -> fail $ "Failed to parse array for " <> name

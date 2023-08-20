{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module RTFDoc.DocParser (
  ToRTF (..),
  FromRTF (..),
  DocParseError (..),
  module X,
  parseDoc,
) where

import Control.Applicative
import Control.Monad.Catch
import Data.Attoparsec.ByteString
import Data.ByteString

-- import RTF.Types

import Control.Arrow (first)
import Control.Monad
import Data.Attoparsec.Combinator as X
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import RTF.Encoding
import RTFDoc.Types

deriving stock instance Show DocParseError
deriving anyclass instance Exception DocParseError

class FromRTF c where
  fromRTF :: c -> [RTFContent]

newtype DocParseError = DocParseError (HasCallStack => Text)

-- CPS parser
newtype DocParserBase e r c = DocParserBase {runDocParserBase :: [RTFContent] -> ((c, [RTFContent]) -> r) -> (e -> r) -> r}

type DocParser = DocParserBase DocParseError

instance Functor (DocParserBase e r) where
  fmap f (DocParserBase p) = DocParserBase $ \content k ke -> p content (k . first f) ke

instance Applicative (DocParserBase e r) where
  pure x = DocParserBase $ \content k _ -> k (x, content)
  DocParserBase fp <*> DocParserBase xp = DocParserBase $ \content k ke -> fp content (\(f, content') -> xp content' (k . first f) ke) ke

instance Alternative (DocParser r) where
  empty = DocParserBase $ \_ _ ke -> ke (DocParseError "empty")
  DocParserBase xp <|> DocParserBase yp = DocParserBase $ \content k ke -> xp content k (\_ -> yp content k ke)

instance MonadPlus (DocParser r)

instance Monad (DocParserBase e r) where
  DocParserBase xp >>= h = DocParserBase $ \content k ke -> xp content (\(x, content') -> (runDocParserBase $ h x) content' k ke) ke

errorWith :: Text -> [RTFContent] -> DocParseError
errorWith msg content = DocParseError $ msg <> "\ncontent" <> defaultShowt content

class ToRTF c where
  toRTF :: DocParser r c

delim2 :: DocParserBase DocParseError r ()
delim2 = isText ";" (== ";") *> pure ()

isControlValue_ :: Text -> (Int -> c) -> DocParser r c
isControlValue_ name f = isControlValue name (\n v -> if name == n then Just (f v) else Nothing)

isControlValue :: Text -> (Text -> Int -> Maybe c) -> DocParser r c
isControlValue msg f = DocParserBase p
 where
  p content@(RTFControlWord name (RTFControlParam value) : content') k ke = case f name value of
    Just result -> k (result, content')
    Nothing -> ke $ errorWith msg content
  p content _ ke = ke $ errorWith msg content

isText :: Text -> (Text -> Bool) -> DocParser r Text
isText msg f = DocParserBase p
 where
  p content@(RTFText text : content') k ke = if f text then k (text, content') else ke (errorWith msg content)
  p content _ ke = ke (errorWith msg content)

isControlGroup :: Text -> DocParser r c -> DocParser r c
isControlGroup msg (DocParserBase p) = DocParserBase g
 where
  g (RTFGroup groupContent : content') k ke = p groupContent (\(x, leftover) -> if Prelude.null leftover then k (x, content') else ke (errorWith msg leftover)) ke
  g content _ ke = ke $ errorWith msg content

isControlLabel :: Text -> (Text -> Maybe c) -> DocParser r c
isControlLabel msg f = DocParserBase g
 where
  g (RTFControlWord n t : rest) k ke
    | t == TrailingSpace || t == NoTrailing, Just c <- f n = k (c, rest)
    | otherwise = ke $ errorWith msg rest
  g content _ ke = ke $ errorWith msg content

instance FromRTF RTFColor where
  fromRTF (RTFColor r g b) = f "red" r <> f "green" g <> f "blue" b
   where
    delim = RTFText ";"
    f n (Just v) = [RTFControlWord n (RTFControlParam (fromIntegral v)), delim]
    f _ Nothing = [delim]

parseDoc :: DocParser (Either DocParseError (c, [RTFContent])) c -> ByteString -> Either DocParseError (c, [RTFContent])
parseDoc (DocParserBase p) dat =
  let
    x = parseOnly (many' $ decodeRTF @RTFContent) dat
   in
    case x of
      Left e -> Left $ DocParseError $ T.pack e
      Right v -> p v Right Left

instance ToRTF RTFColor where
  toRTF = RTFColor <$> red <*> green <*> blue <* delim2
   where
    red = optional $ isControlValue_ "red" (fromIntegral @Int @Word8)
    green = optional $ isControlValue_ "green" (fromIntegral @Int @Word8)
    blue = optional $ isControlValue_ "blue" (fromIntegral @Int @Word8)

instance ToRTF ColorTbl where
  toRTF = ColorTbl <$> isControlGroup "ColorTbl" (isControlLabel "colorTbl" f *> many' toRTF)
   where
    f "colortbl" = Just ()
    f _ = Nothing

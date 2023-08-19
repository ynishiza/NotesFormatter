module RTFDoc.Convert (
  ToRTF (..),
  FromRTF (..),
  dataToRTF,
) where

import Control.Applicative (Alternative, optional)
import Control.Monad.Catch
import Data.Attoparsec.ByteString
import Data.ByteString

-- import RTF.Types

import GHC.Stack (CallStack)
import GHC.Stack.Types (HasCallStack)
import RTF.Encoding
import RTFDoc.Types

newtype ParseError = ParseError (HasCallStack => String)

-- deriving (Show, Eq, Exception)

deriving instance Show ParseError
deriving instance Exception ParseError

dataToRTF :: (ToRTF c, MonadCatch m, Alternative m) => ByteString -> m (c, [RTFContent])
dataToRTF dat = case parseOnly p dat of
  Left msg -> throwM $ ParseError msg
  Right v -> toRTF v
 where
  p = many' (decodeRTF @RTFContent)

class ToRTF c where
  toRTF :: (Alternative m, MonadCatch m) => [RTFContent] -> m (c, [RTFContent])

class FromRTF c where
  fromRTF :: c -> [RTFContent]

instance ToRTF RTFColor where
  toRTF (r : g : b : rest) =
    appendRest rest $ RTFColor <$> red <*> green <*> blue
   where
    red = optional $ isControlValue_ "red" (fromIntegral @Int @Word8) r
    green = optional $ isControlValue_ "green" (fromIntegral @Int @Word8) g
    blue = optional $ isControlValue_ "blue" (fromIntegral @Int @Word8) b
  toRTF _ = throwM $ ParseError ""

instance FromRTF RTFColor where
  fromRTF (RTFColor r g b) = f "red" r <> f "green" g <> f "blue" b
   where
    delim = RTFText ";"
    f n (Just v) = [RTFControlWord n (RTFControlParam (fromIntegral v)), delim]
    f _ Nothing = [delim]

-- toRTF x = undefined

appendRest :: Functor f => a -> f c -> f (c, a)
appendRest a p = (,a) <$> p

isControlValue_ :: MonadCatch m => Text -> (Int -> c) -> RTFContent -> m c
isControlValue_ name f = isControlValue (\n v -> if n == name then Just (f v) else Nothing)

isControlValue :: MonadCatch m => (Text -> Int -> Maybe c) -> RTFContent -> m c
isControlValue f ((RTFControlWord n (RTFControlParam v))) = case f n v of
  Nothing -> throwM $ ParseError ""
  Just c -> return c
isControlValue _ _ = throwM $ ParseError ""

isControlLabel :: MonadCatch m => (Text -> Maybe c) -> RTFContent -> m c
isControlLabel f (RTFControlWord n t)
  | t == TrailingSpace || t == NoTrailing, Just c <- f n = return c
  | otherwise = throwM $ ParseError ""

module RTFDoc.Convert (
  ToRTF(..)
  ) where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString
import RTF.Types
import RTFDoc.Types

class ToRTF c where
  toRTF :: [RTFContent] -> Parser (c, [RTFContent])

instance ToRTF RTFColor where
  toRTF (RTFContentW (RTFControlWord "colortb" _) : r : RTFContentT (RTFText ";") : g : RTFContentT (RTFText ";") : b : rest) =
    appendRest rest $ RTFColor <$> red <*> green <*> blue
   where
    red = optional $ isControlValue_ "red" (fromIntegral @Int @Word8) r
    green = optional $ isControlValue_ "green" (fromIntegral @Int @Word8) g
    blue = optional $ isControlValue_ "blue" (fromIntegral @Int @Word8) b
  toRTF _ = fail ""

-- toRTF x = undefined

appendRest :: Functor f => a -> f c -> f (c, a)
appendRest a p = (,a) <$> p

isControlValue_ :: Text -> (Int -> c) -> RTFContent -> Parser c
isControlValue_ name f = isControlValue (\n v -> if n == name then Just (f v) else Nothing)

isControlValue :: (Text -> Int -> Maybe c) -> RTFContent -> Parser c
isControlValue f (RTFContentW (RTFControlWord n (RTFControlParam v))) = case f n v of
  Nothing -> fail ""
  Just c -> return c
isControlValue _ _ = fail ""

isControlLabel :: (Text -> Maybe c) -> RTFContent -> Parser c
isControlLabel f (RTFContentW (RTFControlWord n t))
  | t == TrailingSpace || t == NoTrailing, Just c <- f n = return c
  | otherwise = fail ""

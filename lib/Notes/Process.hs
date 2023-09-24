module Notes.Process (
  mapColor,
  mapPlainText,
  applyTextMap,
  applyColorMap,
  module X,
) where

import Control.Arrow (second)
import Control.Monad.Reader
import Data.Text qualified as T
import Notes.Config as X
import Notes.File.RTF
import Notes.RTFDoc.Render
import Notes.RTFDoc.Types as X

applyColorMap :: ColorMap -> RTFDoc -> (RTFDoc, (ColorMap, [Int]))
applyColorMap c@ColorMap{..} = second (c,) . mapColor fromColor (toColor, Just toColorSpace)

applyTextMap :: TextMap -> RTFDoc -> (RTFDoc, (TextMap, Int))
applyTextMap t@TextMap{..} = second (t,) . mapPlainText pattern replacement

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor colorToMatch newColorSpec doc@(RTFDoc{..}) = (doc{rtfDocHeader = rtfDocHeader{rtfColors = newColors}}, replacedColorIndexes)
 where
  replaceColorIfMatch (x@(color, _) : rest) =
    let v = if color == colorToMatch then (True, newColorSpec) else (False, x)
     in v : replaceColorIfMatch rest
  replaceColorIfMatch [] = []
  colorResult = replaceColorIfMatch $ rtfColors rtfDocHeader
  replacedColorIndexes =
    fst <$> colorResult
      & zip [0 :: Int ..]
      & filter ((== True) . snd)
      <&> fst
  newColors = snd <$> colorResult

mapPlainText :: Text -> Text -> RTFDoc -> (RTFDoc, Int)
mapPlainText pattern replacement doc@(RTFDoc{..}) = (doc{rtfDocContent = newContent}, mappedCount)
 where
  (mappedCount, newContent) = mapContent (0, rtfDocContent)
  mapContent :: (Int, [RTFContent]) -> (Int, [RTFContent])
  mapContent (count :: Int, RTFText text : rest) =
    let
      (count', text') = if T.isInfixOf pattern text then (count + 1, T.replace pattern replacement text) else (count, text)
     in
      second (RTFText text' :) $ mapContent (count', rest)
  mapContent (count, x : rest) = second (x :) $ mapContent (count, rest)
  mapContent (count, []) = (count, [])

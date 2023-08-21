module Notes.Process (
  mapColor,
  mapPlainText,
) where

import Control.Lens
import Data.Text qualified as T
import Notes.RTFDoc.Types

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor oldColor newColor doc = (set (_rtfDocHeader . _rtfColors) newColors doc, indexes)
 where
  mapToNewColor (x@(c, _) : rest) =
    let v = if c == oldColor then (True, newColor) else (False, x)
     in v : mapToNewColor rest
  mapToNewColor [] = []
  colorResult =
    view (_rtfDocHeader . _rtfColors) doc
      & mapToNewColor
  indexes =
    fst <$> colorResult
      & zip [0 :: Int ..]
      & filter ((== True) . snd)
      <&> fst
  newColors = snd <$> colorResult

mapPlainText :: Text -> Text -> RTFDoc -> (RTFDoc, Int)
mapPlainText pattern replacement doc = (set _rtfDocContent newContent doc, count)
 where
  (count, newContent) = mapContent (0, view _rtfDocContent doc)
  mapContent :: (Int, [RTFContent]) -> (Int, [RTFContent])
  mapContent (i :: Int, RTFText text : rest) =
    let
      (i', text') = if T.isInfixOf pattern text then (i + 1, T.replace pattern replacement text) else (i, text)
      (i'', rest') = mapContent (i', rest)
     in
      (i'', RTFText text' : rest')
  mapContent (i, x : rest) = over _2 (x :) $ mapContent (i, rest)
  mapContent (i, []) = (i, [])

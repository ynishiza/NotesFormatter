module Notes.Process (
  mapColor,
  mapPlainText,
  applyConfig,
  module X,
) where

import Control.Arrow (second)
import Control.Lens
import Data.Text qualified as T
import Notes.Config as X
import Notes.RTFDoc.Types as X

type MapColorResult = [Int]
type MapTextResult = Int

applyConfig :: Config -> RTFDoc -> (RTFDoc, [MapColorResult], [MapTextResult])
applyConfig Config{..} doc =
  let
    (doc', r1) = cols doc
    (doc'', r2) = txts doc'
   in
    (doc'', r1, r2)
 where
  cols d = foldr (\c (d', res) -> second (: res) (apCol c d')) (d, []) cfgColorMap
  txts d = foldr (\c (d', res) -> second (: res) (apText c d')) (d, []) cfgTextMap
  apCol ColorMap{..} = mapColor fromColor (toColor, Just toColorSpace)
  apText TextMap{..} = mapPlainText pattern replacement

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, MapColorResult)
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

mapPlainText :: Text -> Text -> RTFDoc -> (RTFDoc, MapTextResult)
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

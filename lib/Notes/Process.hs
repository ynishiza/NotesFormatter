module Notes.Process (
  mapColor,
  mapPlainText,
  mapFontName,
  applyTextMap,
  applyColorMap,
  applyFontMap,
  module X,
) where

import Control.Arrow (second)
import Control.Lens (view)
import Control.Monad.Reader
import Data.Text qualified as T
import Notes.Config as X
import Notes.File.RTF
import Notes.RTFDoc.Render
import Notes.RTFDoc.Types as X
import Debug.Trace (trace)

applyColorMap :: ColorMap -> RTFDoc -> (RTFDoc, (ColorMap, [Int]))
applyColorMap c@ColorMap{..} = second (c,) . mapColor fromColor (toColor, Just toColorSpace)

applyTextMap :: TextMap -> RTFDoc -> (RTFDoc, (TextMap, Int))
applyTextMap t@TextMap{..} = second (t,) . mapPlainText pattern replacement

applyFontMap :: FontMap -> RTFDoc -> (RTFDoc, (FontMap, [Int]))
applyFontMap t@FontMap{..} = second (t,) . mapFontName fromFontName toFontName

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor colorToMatch newColorSpec doc@(RTFDoc{..}) = (doc{rtfDocHeader = rtfDocHeader{rtfColors = newColors}}, replacedColorIndexes)
 where
  (newColors, replacedColorIndexes) = mapMatches' f (rtfColors rtfDocHeader)
   where
    f (color, _) =
      if color == colorToMatch
        then Just newColorSpec
        else Nothing

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

mapFontName :: Text -> Text -> RTFDoc -> (RTFDoc, [Int])
mapFontName oldName newName doc@(RTFDoc{..}) = trace (show newFonts) 
  ( doc{rtfDocHeader = rtfDocHeader{rtfFontTbl = FontTbl newFonts}}
  , mappedIndexes
  )
 where
  (newFonts, mappedIndexes) = mapMatches' f (view (_rtfDocHeader . _rtfFontTbl . _FontTbl) doc)
   where
    f Nothing = Nothing
    f (Just fontInfo) =
      trace (show fontInfo <> show (oldName == fontName fontInfo) <> T.unpack newName) $ Just $
        if fontName fontInfo == oldName
          then Just fontInfo{fontName = newName}
          else Just fontInfo

mapMatches' :: (a -> Maybe a) -> [a] -> ([a], [Int])
mapMatches' f list = (snd <$> result, indexes)
 where
  result = mapMatches f list
  indexes =
    zip [0 ..] (fst <$> result)
      & filter ((== True) . snd)
      & (fst <$>)

mapMatches :: (a -> Maybe a) -> [a] -> [(Bool, a)]
mapMatches _ [] = []
mapMatches f (v : rest) =
  let result = case f v of
        Just v' -> (True, v')
        Nothing -> (False, v)
   in result : mapMatches f rest

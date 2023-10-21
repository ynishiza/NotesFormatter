{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Notes.Process (
  mapColor,
  mapPlainText,
  mapContents,
  mapFontName,
  applyTextMap,
  applyColorMap,
  applyFontMap,
  applyContentMap,
  ProcessError (..),
  module X,
) where

import Control.Arrow (second)
import Control.Exception (Exception)
import Control.Lens (view)
import Control.Monad.Reader
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Data.List.NonEmpty qualified as N
import Data.Text qualified as T
import Notes.Config as X
import Notes.File.RTF
import Notes.RTFDoc.Render
import Notes.RTFDoc.Types as X

data ProcessError = TextMapError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

applyColorMap :: ColorMap -> RTFDoc -> (RTFDoc, (ColorMap, [Int]))
applyColorMap c@ColorMap{..} = second (c,) . mapColor fromColor (toColor, Just toColorSpace)

applyTextMap :: TextMap -> RTFDoc -> Either ProcessError (RTFDoc, (TextMap, Int))
applyTextMap t@TextMap{..} = (second (t,) <$>) . mapPlainText pattern replacement

applyContentMap :: ContentMap -> RTFDoc -> (RTFDoc, (ContentMap, Int))
applyContentMap m@ContentMap{..} = second (m,) . mapContents fromContents toContents

applyFontMap :: FontMap -> RTFDoc -> (RTFDoc, (FontMap, [Int]))
applyFontMap t@FontMap{..} = second (t,) . mapFontName fromFontName toFont

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor colorToMatch newColorSpec doc@(RTFDoc{..}) = (doc{rtfDocHeader = rtfDocHeader{rtfColors = newColors}}, replacedColorIndexes)
 where
  (newColors, replacedColorIndexes) = mapMatches' f (rtfColors rtfDocHeader)
   where
    f (color, _)
      | color == colorToMatch = Just newColorSpec
      | otherwise = Nothing

mapPlainText :: Text -> Text -> RTFDoc -> Either ProcessError (RTFDoc, Int)
mapPlainText "" _ _ = Left $ TextMapError "Text map pattern cannot be empty"
mapPlainText pattern replacement doc@(RTFDoc{..}) = Right (doc{rtfDocContent = newContent}, mappedCount)
 where
  (mappedCount, newContent) = mapContent (0, rtfDocContent)
  mapContent :: (Int, [RTFDocContent]) -> (Int, [RTFDocContent])
  mapContent (count :: Int, ContentText text : rest) =
    let
      (count', text')
        | T.isInfixOf pattern text = (count + 1, T.replace pattern replacement text)
        | otherwise = (count, text)
     in
      second (ContentText text' :) $ mapContent (count', rest)
  mapContent (count, x : rest) = second (x :) $ mapContent (count, rest)
  mapContent (count, []) = (count, [])

mapContents :: NonEmpty RTFDocContent -> NonEmpty RTFDocContent -> RTFDoc -> (RTFDoc, Int)
mapContents (x :| xs) toContents doc@RTFDoc{..} =
  ( doc{rtfDocContent = intercalate (N.toList toContents) sp}
  , length sp - 1
  )
 where
  sp = splitOn (x : xs) rtfDocContent

mapFontName :: Text -> FontMapFont -> RTFDoc -> (RTFDoc, [Int])
mapFontName oldName (FontMapFont{..}) doc@(RTFDoc{..}) =
  ( doc{rtfDocHeader = rtfDocHeader{rtfFontTbl = FontTbl newFonts}}
  , mappedIndexes
  )
 where
  (newFonts, mappedIndexes) = mapMatches' f (view (_rtfDocHeader . _rtfFontTbl . _FontTbl) doc)
   where
    f Nothing = Nothing
    f (Just fontInfo) =
      if fontName fontInfo == oldName
        then Just $ Just fontInfo{fontFamily = fmFamily, fontName = fmFontName}
        else Nothing

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

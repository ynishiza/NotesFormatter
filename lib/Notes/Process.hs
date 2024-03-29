{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Notes.Process (
  mapColor,
  mapPlainText,
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
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Data.List.NonEmpty qualified as N
import Data.Maybe
import Data.Text qualified as T
import Notes.Config as X
import Notes.File.RTF
import Notes.RTFDoc.Render
import Notes.RTFDoc.Types as X

data ProcessError
  = TextMapError String
  | FontMapError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

applyColorMap :: ColorMap -> RTFDoc -> (RTFDoc, (ColorMap, [Int]))
applyColorMap c@ColorMap{..} = second (c,) . mapColor fromColor (toColor, Just toColorSpace)

applyTextMap :: TextMap -> RTFDoc -> Either ProcessError (RTFDoc, (TextMap, Int))
applyTextMap t@TextMap{..} = (second (t,) <$>) . mapPlainText pattern replacement

applyContentMap :: ContentMap -> RTFDoc -> Either ProcessError (RTFDoc, (ContentMap, Int))
applyContentMap m@ContentMap{..} d@RTFDoc{..} = case annotateWithContext rtfDocHeader emptyContext rtfDocContent of
  Left e -> Left e
  Right contents' ->
    let
      (result, c) = mapContentsForCharset contentCharset fromContents toContents contents'
      d' = d{rtfDocContent = snd <$> result}
     in
      Right (d', (m, c))

applyFontMap :: FontMap -> RTFDoc -> Either ProcessError (RTFDoc, (FontMap, [Int]))
applyFontMap t@FontMap{..} = (second (t,) <$>) . mapFontName fromFontName toFont

mapColor :: RTFColor -> (RTFColor, Maybe ColorSpace) -> RTFDoc -> (RTFDoc, [Int])
mapColor colorToMatch newColorSpec doc@(RTFDoc{..}) = (doc{rtfDocHeader = rtfDocHeader{rtfColors = newColors}}, replacedColorIndexes)
 where
  (newColors, replacedColorIndexes) = extractMatchIndexes $ mapMatches f (rtfColors rtfDocHeader)
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

mapContentsForCharset :: Int -> NonEmpty RTFDocContent -> NonEmpty RTFDocContent -> [(DocContext, RTFDocContent)] -> ([(DocContext, RTFDocContent)], Int)
mapContentsForCharset _ _ _ [] = ([], 0)
mapContentsForCharset charset fromContents toContents contents = (skipped ++ zip (fst <$> toMap) mapped ++ rest, c + c')
 where
  isMatchingCharset (Just FontInfo{..}) = fromMaybe 0 fontCharset == charset
  isMatchingCharset _ = charset == 0
  (skipped, contents') = break (isMatchingCharset . ctxFontInfo . fst) contents
  (toMap, contents'') = span (isMatchingCharset . ctxFontInfo . fst) contents'
  (mapped, c) = mapContentsRaw fromContents toContents (snd <$> toMap)
  (rest, c') = mapContentsForCharset charset fromContents toContents contents''

mapContentsRaw :: NonEmpty RTFDocContent -> NonEmpty RTFDocContent -> [RTFDocContent] -> ([RTFDocContent], Int)
mapContentsRaw (x :| xs) toContents contents =
  ( intercalate (N.toList toContents) sp
  , length sp - 1
  )
 where
  sp = splitOn (x : xs) contents

mapFontName :: Text -> FontMapFont -> RTFDoc -> Either ProcessError (RTFDoc, [Int])
mapFontName oldName mapping@(FontMapFont{..}) doc@(RTFDoc{..}) = do
  (newFonts, mappedIndexes) <- result
  return
    ( doc{rtfDocHeader = rtfDocHeader{rtfFontTbl = FontTbl newFonts}}
    , mappedIndexes
    )
 where
  result = extractMatchIndexes <$> mapMatchesBase match (view (_rtfDocHeader . _rtfFontTbl . _FontTbl) doc)
   where
    match :: Maybe FontInfo -> Either ProcessError (Maybe (Maybe FontInfo))
    match Nothing = Right Nothing
    match (Just fontInfo)
      -- case: charsets MUST match.
      -- Different charsets have different encoding schemes for escaped symbols
      -- e.g.
      --      charset = 128    RTF = \'82\'a0      character あ
      --      charset = 0      RTF = \'82\'a0      meaningless?
      --
      -- Hence the mapping shouldn't change the charset.
      | fontName fontInfo == oldName && fontCharset fontInfo /= fmCharset =
          Left
            $ FontMapError
            $ "Charset mismatch mapping "
            <> show fontInfo
            <> " to "
            <> show mapping
            <> ".\n Changing the charset is not allowed since this may break encoding of special symbols"
      | fontName fontInfo == oldName && fontCharset fontInfo == fmCharset =
          Right $ Just $ Just fontInfo{fontFamily = fmFamily, fontName = fmFontName}
      | otherwise = Right Nothing

-- [Int] = indexes where the value was True
extractMatchIndexes :: [(Bool, a)] -> ([a], [Int])
extractMatchIndexes list = (snd <$> list, indexes)
 where
  indexes =
    zip [0 ..] (fst <$> list)
      & filter snd
      & (fst <$>)

mapMatches :: (a -> Maybe a) -> [a] -> [(Bool, a)]
mapMatches f = fromRight [] . mapMatchesBase (Right . f)

mapMatchesBase :: (a -> Either e (Maybe a)) -> [a] -> Either e [(Bool, a)]
mapMatchesBase _ [] = Right []
mapMatchesBase f (v : rest) =
  case f v of
    Right v' ->
      let result = case v' of
            Just v'' -> (True, v'')
            Nothing -> (False, v)
       in (result :) <$> mapMatchesBase f rest
    Left e -> Left e

data DocContext = DocContext
  { ctxFontInfo :: Maybe FontInfo
  }

emptyContext :: DocContext
emptyContext = DocContext Nothing

annotateWithContext :: RTFHeader -> DocContext -> [RTFDocContent] -> Either ProcessError [(DocContext, RTFDocContent)]
annotateWithContext _ _ [] = Right []
annotateWithContext header@RTFHeader{..} ctx (c@(ContentControlWord NoPrefix "f" (RTFControlParam p)) : rest) = ((ctx', c) :) <$> annotateWithContext header ctx' rest
 where
  FontTbl infos = rtfFontTbl
  fontInfo = infos !! p
  ctx' = ctx{ctxFontInfo = fontInfo}
annotateWithContext header ctx (c : rest) = ((ctx, c) :) <$> annotateWithContext header ctx rest

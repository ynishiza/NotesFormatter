{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module RTFDoc.CPSParser (
  DocParserState,
  DocParser,
  parseDoc,
  module X,
  rtfControlWordLabel_,
  rtfControlWordLabel,
  rtfText,
  rtfText_,
  rtfGroup,
  rtfControlWord,
  rtfControlWordValue_,
  rtfControlWordValue,
  rtfSymbol,
  rtfSymbol_,
) where

import CPSParser.Types as X
import Control.Monad.Identity
import Data.Attoparsec.ByteString
import Data.ByteString
import Data.Text qualified as T
import RTF.Convert

type DocParserState = (Int, [RTFContent])

type DocParseError = CParserError DocParserState

type DocParser r = CParserT DocParseError DocParserState r Identity

errorWith :: Text -> DocParserState -> CParserError DocParserState
errorWith msg state = CParserError state $ msg <> "\ncontent" <> defaultShowt state

rtfControlWordValue_ :: Text -> (Int -> c) -> DocParser r c
rtfControlWordValue_ name f = rtfControlWordValue name (\n v -> if name == n then Just (f v) else Nothing)

rtfControlWordValue :: Text -> (Text -> Int -> Maybe c) -> DocParser r c
rtfControlWordValue msg f = rtfControlWord msg f'
 where
  f' name (RTFControlParam v) = f name v
  f' _ _ = Nothing

rtfText_ :: Text -> DocParser r Text
rtfText_ text = rtfText text $ \t -> if t == text then Just text else Nothing

rtfText :: Text -> (Text -> Maybe Text) -> DocParser r Text
rtfText msg f = CParserT p
 where
  msg' = "[RTFText]" <> msg
  p state@(i, RTFText text : content') k ke = case f text of
    Just text' -> k (text', (i + 1, content'))
    Nothing -> ke (errorWith msg' state)
  p state _ ke = ke (errorWith msg' state)

rtfGroup :: Text -> DocParser r c -> DocParser r c
rtfGroup msg (CParserT p) = CParserT g
 where
  msg' = "[RTFGroup]" <> msg
  g (i, RTFGroup groupContent : content') k ke =
    p
      (i + 1, groupContent)
      ( \(x, s@(j, leftover)) ->
          if Prelude.null leftover
            then k (x, (j + 1, content'))
            else ke (errorWith (msg' <> " Group did not consume all") s)
      )
      ke
  g state _ ke = ke $ errorWith (msg' <> " not a group") state

rtfControlWordLabel_ :: Text -> DocParser r Text
rtfControlWordLabel_ name = rtfControlWordLabel name (\v -> if v == name then Just v else Nothing)

rtfControlWordLabel :: Text -> (Text -> Maybe c) -> DocParser r c
rtfControlWordLabel msg f = rtfControlWord msg f'
 where
  f' name end
    | end == TrailingSpace || end == NoTrailing = f name
    | otherwise = Nothing

rtfControlWord :: Text -> (Text -> RTFControlWordEnd -> Maybe c) -> DocParser r c
rtfControlWord msg f = CParserT p
 where
  msg' = "[RTFControlWord]" <> msg
  p state@(i, RTFControlWord n t : rest) k ke = case f n t of
    Just v -> k (v, (i + 1, rest))
    Nothing -> ke $ errorWith msg' state
  -- \| t == TrailingSpace || t == NoTrailing, Just c <- f n = k (c, (i + 1, rest))
  -- \| otherwise = ke $ errorWith msg' state
  p state _ ke = ke $ errorWith msg' state

rtfSymbol_ :: Char -> DocParser r Char
rtfSymbol_ c = rtfSymbol (T.pack [c]) $ \v -> if v == c then Just v else Nothing

rtfSymbol :: Text -> (Char -> Maybe c) -> DocParser r c
rtfSymbol msg f = CParserT p
 where
  msg' = "[RTFControlSymbol]" <> msg
  p state@(i, RTFControlSymbol c : rest) k ke = case f c of
    Just v -> k (v, (i + 1, rest))
    Nothing -> ke $ errorWith msg state
  p state _ ke = ke $ errorWith msg' state

parseDoc :: DocParser (Either DocParseError (c, DocParserState)) c -> ByteString -> Either DocParseError (c, [RTFContent])
parseDoc p dat =
  let
    x = parseOnly (many' $ trimNewLines parseRTFContent) dat
   in
    case x of
      Left e -> Left $ CParserError (0, []) $ T.pack e
      Right v -> case runIdentity (execCParser p (0, v)) of
        Left e -> Left e
        Right (result, (_, z)) -> Right (result, z)

{-# OPTIONS_GHC -Wno-orphans #-}

module Notes.RTF.ContentParser (
  ContentParser,
  RTFParseError (..),
  --
  rtfText,
  rtfText_,
  rtfSymbol,
  rtfSymbol_,
  rtfControlWord,
  rtfControlWordLabel,
  rtfControlWordLabel_,
  rtfControlWordValue,
  rtfControlWordValue_,
  rtfGroup,
  errorToken,
  errorLabelString,
  errorLabelText,
  Void,
  _stateInput,
) where

import Control.Lens
import Data.List (intercalate)
import Data.List.NonEmpty qualified as N
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void (Void)
import Debug.Trace (trace)
import Notes.RTF.Convert
import Text.Megaparsec

data RTFParseError = RTFGroupError Int RTFContent (ParseErrorBundle [RTFContent] RTFParseError)
  deriving (Show, Eq)

instance Ord RTFParseError where
  RTFGroupError n _ _ <= RTFGroupError m _ _ = n <= m

type ContentParser = ParsecT RTFParseError [RTFContent] Identity

$(makeLensesWith dataLensRules ''State)
$(makeLensesWith dataLensRules ''SourcePos)
$(makeLensesWith dataLensRules ''PosState)

enableDebug :: Bool
-- enableDebug = True
enableDebug = False

trace_ :: String -> a -> a
trace_ = if enableDebug then trace else (\_ x -> x)

deriving instance Ord RTFControlSuffix
deriving instance Ord RTFControlPrefix
deriving instance Ord RTFContent

instance VisualStream [RTFContent] where
  showTokens _ = showTokens_ . N.toList
  tokensLength p s = length $ showTokens p s

instance ShowErrorComponent RTFParseError where
  showErrorComponent (RTFGroupError _ g e) =
    errorBundlePretty e
      & dropWhile (/= ':')
      & (header <>)
   where
    header =
      "\n"
        <> show g

showToken :: RTFContent -> String
showToken = show

showTokens_ :: [RTFContent] -> String
showTokens_ = intercalate "," . (showToken <$>)

instance TraversableStream [RTFContent] where
  reachOffset newOffset p@PosState{..} =
    trace_
      debugInfo
      ( Just (showTokens_ lineText <> lineTrailing)
      , PosState
          { pstateInput = unconsumed
          , pstateOffset = newOffset
          , pstateSourcePos = newSourcePos
          , pstateTabWidth = mkPos 0
          , pstateLinePrefix = ""
          }
      )
   where
    SourcePos{..} = pstateSourcePos
    idx = newOffset - pstateOffset
    (consumed, unconsumed) = splitAt idx pstateInput
    lineText = consumed <> take 10 unconsumed
    lineTrailing = if length unconsumed > 10 then "..." else ""
    headTokens = take idx lineText
    newSourcePos =
      SourcePos
        { sourceName = "RTF"
        , sourceLine = sourceLine
        , sourceColumn = mkPos $ sum (length . showToken <$> headTokens) + length headTokens + 1
        }
    debugInfo =
      "\ninput (offset, PosState):\t"
        <> show (newOffset, p)
        <> "\n(consumed, line):\t\t"
        <> show (consumed, lineText)
        <> "\nnew PosState:\t\t"
        <> show newSourcePos

rtfSymbol_ :: Char -> ContentParser Char
rtfSymbol_ symbol = rtfSymbol (\x -> if x == symbol then Right symbol else Left (errorToken (RTFControlSymbol symbol)))

rtfSymbol :: (Char -> Either (ErrorItem RTFContent) c) -> ContentParser c
rtfSymbol f = try $ do
  x@(RTFControlSymbol c) <- satisfyWith "RTFControlSymbol" (has _RTFControlSymbol)
  valueOrError x (f c)

rtfControlWordValue_ :: Text -> (Int -> c) -> ContentParser c
rtfControlWordValue_ name f = rtfControlWordValue (\n v -> if name == n then Right (f v) else Left (errorLabelText ("RTFControlSymbol " <> name)))

rtfControlWordValue :: (Text -> Int -> Either (ErrorItem RTFContent) c) -> ContentParser c
rtfControlWordValue f = rtfControlWord f'
 where
  f' _ name (RTFControlParam v) = f name v
  f' _ name _ = Left $ errorLabelText $ "RTFControlWord * " <> name <> " (RTFControlParam *)"

rtfControlWordLabel_ :: Text -> ContentParser Text
rtfControlWordLabel_ name = rtfControlWordLabel (\x -> if x == name then Right x else Left (errorLabelText $ "RTFControlWord " <> name))

rtfControlWordLabel :: (Text -> Either (ErrorItem RTFContent) c) -> ContentParser c
rtfControlWordLabel f = rtfControlWord f'
 where
  f' _ name end
    | end == SpaceSuffix || end == NoSuffix = f name
    | otherwise = Left $ errorLabelText $ "RTFControlWord " <> name

rtfControlWord :: (RTFControlPrefix -> Text -> RTFControlSuffix -> Either (ErrorItem RTFContent) c) -> ContentParser c
rtfControlWord f = try $ do
  x@(RTFControlWord prefix n suffix) <- satisfyWith "RTFControlWord" (has _RTFControlWord)
  valueOrError x (f prefix n suffix)

rtfText_ :: Text -> ContentParser Text
rtfText_ text = rtfText (\x -> if x == text then Right text else Left (errorToken $ RTFText text))

rtfText :: (Text -> Either (ErrorItem RTFContent) Text) -> ContentParser Text
rtfText f = try $ do
  x@(RTFText t) <- satisfyWith "RTFText" (has _RTFText)
  valueOrError x (f t)

rtfGroup :: Text -> ContentParser c -> ContentParser c
rtfGroup msg p = try $ do
  (RTFGroup g) <- satisfyWith "RTFGroup" (has _RTFGroup)
  offset <- getOffset
  case runParser (p <* eof) (T.unpack msg) g of
    Left e -> parseError $ FancyError (offset - 1) $ S.singleton $ ErrorCustom $ RTFGroupError offset (RTFGroup g) e
    Right v -> return v

valueOrError :: RTFContent -> Either (ErrorItem RTFContent) c -> ContentParser c
valueOrError _ (Right v) = return v
valueOrError unexpectedTerm (Left e) = do
  o <- getOffset
  parseError $ TrivialError (o - 1) (Just $ errorToken unexpectedTerm) (S.singleton e)

errorLabelString :: String -> ErrorItem t
errorLabelString "" = error "errorLabelString: error label must not be empty"
errorLabelString s = Label $ N.fromList s

errorLabelText :: Text -> ErrorItem t
errorLabelText = errorLabelString . T.unpack

errorToken :: t -> ErrorItem t
errorToken = Tokens . N.singleton

satisfyWith :: Text -> (RTFContent -> Bool) -> ContentParser RTFContent
satisfyWith expected = region mapError . satisfy
 where
  mapError (TrivialError offset unexpectedValue _) = TrivialError offset unexpectedValue $ S.singleton $ errorLabelText expected
  mapError e = e

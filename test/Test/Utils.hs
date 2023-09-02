module Test.Utils (
  multiline,
  rtfPath,
  normalizeRTFForTest,
  expectToRTFDocSuccess,
  module X,
) where

import Data.Text qualified as T
import Language.Haskell.TH.Quote
import Notes.RTFDoc as X
import Test.Hspec

rtfPath :: FilePath
rtfPath = basePath </> "data" </> "rtf"

multiline :: QuasiQuoter
multiline =
  QuasiQuoter
    { quoteExp = \s -> [|T.pack s|]
    , quoteDec = undefined
    , quoteType = undefined
    , quotePat = undefined
    }

-- Note: newlines are insignificant in RTF.
-- Hence, normalize the RTF text by removing new lines.
-- However, the content text may have newlines, represented as an
-- RTF symbol
--      \\n
-- To avoid replacing the newline RTF symbol, replace the symbol with a placeholder first.
normalizeRTFForTest :: Text -> Text
normalizeRTFForTest text =
  text
    & T.replace "\\\\\n" "\\\\" -- TODO: WHY is this needed? Otherwise "TeX-LaTeX tricks.rtfd" fails
    & T.replace "\\\n" newlinePlaceholder
    & T.replace "\n" ""
    & T.replace "\r" ""
    -- Note: use placeholder for whitespace to highlight diff more clearly.
    & T.replace " " whitespacePlaholder
    & T.replace newlinePlaceholder " NL\n"
 where
  whitespacePlaholder = " _ "
  newlinePlaceholder = "😄"

expectToRTFDocSuccess :: ToRTFDoc a => Text -> IO a
expectToRTFDocSuccess d = do
  let result = parseDoc_ toRTFDoc d
  case result of
    Left msg -> do
      expectationFailure $ show msg
      fail $ show msg
    Right v -> return v

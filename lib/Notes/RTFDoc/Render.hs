{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module Notes.RTFDoc.Render (
  Renderable (..),
  module X,
) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Text qualified as T
import GHC.Exts (IsString)
import Notes.ParserUtils
import Notes.RTF.Render as X
import Notes.RTFDoc.Types as X
import Prelude hiding (takeWhile)

class Renderable c where
  render :: c -> Text

endWithNewLine :: (IsString s, Monoid s) => s -> s
endWithNewLine = (<> "\n")

instance Renderable RTFDoc where
  render RTFDoc{..} =
    "{"
      <> endWithNewLine (render rtfDocHeader)
      <> render rtfDocContent
      <> "}"

instance Renderable n => Renderable [n] where
  render content = T.intercalate "" $ render <$> content

instance Renderable RTFDocContent where
  render (ContentEscapedSequence n) = T.pack $ "\\" <> [escapedSequenceChar] <> escapedSequenceRenderHex n
  render (ContentControlSymbol c) = renderRTFElement (RTFControlSymbol c)
  render (ContentControlWord prefix name suffix) = renderRTFElement (RTFControlWord prefix name suffix)
  render (ContentText text) = renderRTFElement (RTFText text)
  render (ContentGroup g) = renderRTFGroup $ T.intercalate "" (render <$> g)

instance Renderable RTFHeader where
  render (RTFHeader{..}) =
    renderControlWithLabel_ "rtf1"
      <> render rtfCharset
      <> T.intercalate "" (render <$> rtfCocoaControls)
      <> endWithNewLine (render rtfFontTbl)
      <> endWithNewLine (render (ColorTbl (fst <$> rtfColors)))
      <> endWithNewLine (render (ExpandedColorTbl (snd <$> rtfColors)))

instance Renderable Charset where
  render (Ansi n) =
    renderControlWithLabel_ "ansi"
      <> renderControlWithValue "ansicpg" n

instance Renderable CocoaControl where
  render (CocoaControl name (Just v)) = renderControlWithValue ("cocoa" <> name) v
  render (CocoaControl name Nothing) = renderControlWithLabel_ ("cocoa" <> name)

instance Renderable FontTbl where
  render (FontTbl infos) =
    renderRTFGroup $
      renderControlWithLabel_ "fonttbl"
        <> T.intercalate "" (renderRTFInnerControl <$> infos)

instance Renderable ColorTbl where
  render (ColorTbl colors) =
    renderRTFGroup $
      renderControlWithLabel_ "colortbl"
        <> T.intercalate "" (appendBlockEnd . render <$> colors)

instance Renderable ExpandedColorTbl where
  render (ExpandedColorTbl colors) =
    renderRTFGroup $
      renderControlWithLabel StarPrefix "expandedcolortbl" NoSuffix
        <> T.intercalate "" (renderRTFInnerControl <$> colors)

instance Renderable FontInfo where
  render (FontInfo fontId family charset name) =
    renderControlWithValue "f" fontId
      <> render family
      <> maybe "" (renderControlWithValue "fcharset") charset
      <> " "
      <> name

instance Renderable FontFamily where
  render f = renderRTFElement $ RTFControlWord NoPrefix (fontFamilyText f) NoSuffix

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance Renderable RTFColor where
  render (RTFColor r g b) = renderValue "red" r <> renderValue "green" g <> renderValue "blue" b
   where
    renderValue name (Just value) = renderControlWithValue name value
    renderValue _ Nothing = ""

instance Renderable ColorSpace where
  render word = case word of
    CSGray v -> renderControlWithLabel_ "csgray" <> value v
    CSSRGB r g b a -> renderControlWithLabel_ "cssrgb" <> value r <> value g <> value b <> maybe "" value a
    CSGenericRGB r g b a -> renderControlWithLabel_ "csgenericrgb" <> value r <> value g <> value b <> maybe "" value a
   where
    value = renderControlWithValue "c"

renderControlWithLabel_ :: Text -> Text
renderControlWithLabel_ name = renderControlWithLabel NoPrefix name NoSuffix

renderControlWithLabel :: RTFControlPrefix -> Text -> RTFControlSuffix -> Text
renderControlWithLabel prefix name suffix = renderRTFElement $ RTFControlWord prefix name suffix

renderControlWithValue :: Integral v => Text -> v -> Text
renderControlWithValue name v = renderRTFElement $ RTFControlWord NoPrefix name $ RTFControlParam $ fromIntegral v

appendBlockEnd :: Text -> Text
appendBlockEnd text = text <> charBlockEnd

renderRTFInnerControl :: Renderable c => Maybe c -> Text
renderRTFInnerControl (Just item) = render item <> charBlockEnd
renderRTFInnerControl Nothing = charBlockEnd

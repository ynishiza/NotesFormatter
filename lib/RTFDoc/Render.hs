{-
  RTF 15 spec: https://www.biblioscape.com/rtf15_spec.htm
  Apple's extensions: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/AttributedStrings/Tasks/RTFAndAttrStrings.html#//apple_ref/doc/uid/20000164-155922
-}

module RTFDoc.Render (
  Renderable (..),
  module X,
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as P
import Data.Functor
import Data.Text qualified as T
import RTF.Convert as X
import RTFDoc.Types as X
import Prelude hiding (takeWhile)

class Renderable c where
  render :: c -> Text

instance Renderable RTFDoc where
  render RTFDoc{..} =
    "{"
      <> render rtfDocHeader
      <> "\n"
      <> T.intercalate "" (renderRTFContent <$> rtfDocContent)
      <> "}"

instance Renderable RTFHeader where
  render (RTFHeader{..}) =
    encodeControlWithLabel "rtf1" False
      <> render rtfCharset
      <> T.intercalate "" (render <$> rtfCocoaControls)
      <> render rtfFontTbl
      <> render (ColorTbl (fst <$> rtfColors))
      <> render (ExpandedColorTbl (snd <$> rtfColors))

instance Renderable Charset where
  render (Ansi n) =
    encodeControlWithLabel "ansi" False
      <> encodeControlWithValue "ansicpg" n

instance Renderable CocoaControl where
  render (CocoaControl name (Just v)) = encodeControlWithValue ("cocoa" <> name) v
  render (CocoaControl name Nothing) = encodeControlWithLabel ("cocoa" <> name) False

instance Renderable FontTbl where
  render (FontTbl infos) =
    renderRTFGroup $
      encodeControlWithLabel "fonttbl" False
        <> T.intercalate "" (encodeRTFInnerControl <$> infos)

instance Renderable ColorTbl where
  render (ColorTbl colors) =
    renderRTFGroup $
      encodeControlWithLabel "colortbl" False
        <> T.intercalate "" (appendBlockEnd . render <$> colors)

instance Renderable ExpandedColorTbl where
  render (ExpandedColorTbl colors) =
    renderRTFGroup $
      toDestination (encodeControlWithLabel "expandedcolortbl" False)
        <> T.intercalate "" (encodeRTFInnerControl <$> colors)

instance Renderable FontInfo where
  render (FontInfo fontId family charset name) =
    encodeControlWithValue "f" fontId
      <> render family
      <> maybe "" (encodeControlWithValue "fcharset") charset
      <> " "
      <> name

instance Renderable FontFamily where
  render f = renderRTFContent $ RTFControlWord (fontFamilyText f) NoTrailing

fontFamilyText :: FontFamily -> Text
fontFamilyText = T.toLower . T.pack . show

instance Renderable RTFColor where
  render (RTFColor r g b) = encode "red" r <> encode "green" g <> encode "blue" b
   where
    encode name (Just value) = encodeControlWithValue name value
    encode _ Nothing = ""

instance Renderable ColorSpace where
  render word = case word of
    CSGray v -> encodeControlWithLabel "csgray" False <> value v
    CSSRGB r g b -> encodeControlWithLabel "cssrgb" False <> value r <> value g <> value b
    CSGenericRGB r g b -> encodeControlWithLabel "csgenericrgb" False <> value r <> value g <> value b
   where
    value = encodeControlWithValue "c"

encodeControlWithLabel :: Text -> Bool -> Text
encodeControlWithLabel name False = renderRTFContent $ RTFControlWord name NoTrailing
encodeControlWithLabel name True = renderRTFContent $ RTFControlWord name TrailingSpace

encodeControlWithValue :: Integral v => Text -> v -> Text
encodeControlWithValue name v = renderRTFContent $ RTFControlWord name $ RTFControlParam $ fromIntegral v

toDestination :: Text -> Text
toDestination t = "\\*" <> t

appendBlockEnd :: Text -> Text
appendBlockEnd text = text <> charBlockEnd

encodeRTFInnerControl :: Renderable c => Maybe c -> Text
encodeRTFInnerControl (Just item) = render item <> charBlockEnd
encodeRTFInnerControl Nothing = charBlockEnd

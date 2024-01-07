{-# LANGUAGE QuasiQuotes #-}

module Test.Data.StyleRTF (
  content,
  contentText,
) where

import Notes.RTFDoc

contentText :: Text
contentText =
  [multiline|
{\rtf1\ansi\ansicpg1252\cocoartf2639\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\froman\fcharset0 TimesNewRomanPSMT;\f1\fnil\fcharset0 Monaco;\f2\fnil\fcharset0 HelveticaNeue-Bold;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red230\green230\blue230;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\csgray\c100000;\cssrgb\c1\c2\c3;}

{\info{\author Yui Nishizawa}}\vieww11520\viewh8400\viewkind0\deftab720\pard\pardeftab720\partightenfactor0\f0\fs28 \cf2 \cb3 \expnd0\expndtw0\kerning0Normbl text\
\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\f1 \cf2 \cb4 \
Code block\
\
\f0 \cf0 \cb1 \kerning1\expnd0\expndtw0 \
\
\f2\b Bold text
\f0\b0 \
\ul Underline text\
\ulnone \strike \strikec0 Strikethrough\strike0\striked0 \
\
\fs96 size 48\
\fs24 \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\qj\partightenfactor0\cf0 justify\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 left blign\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\qc\partightenfactor0\cf0 center blign\
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\qr\partightenfactor0\cf0 right blign
\fs28 \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 \
}
|]

content :: RTFDoc
content =
  RTFDoc
    ( RTFHeader
        { rtfCharset = Ansi 1252
        , rtfCocoaControls = [CocoaControl "rtf" (Just 2639), CocoaControl "textscaling" (Just 0), CocoaControl "platform" (Just 0)]
        , rtfFontTbl =
            FontTbl
              [ Just
                  FontInfo
                    { fontNum = 0
                    , fontFamily = FRoman
                    , fontCharset = Just 0
                    , fontName = "TimesNewRomanPSMT"
                    }
              , Just
                  FontInfo
                    { fontNum = 1
                    , fontFamily = FNil
                    , fontCharset = Just 0
                    , fontName = "Monaco"
                    }
              , Just
                  FontInfo
                    { fontNum = 2
                    , fontFamily = FNil
                    , fontCharset = Just 0
                    , fontName = "HelveticaNeue-Bold"
                    }
              ]
        , rtfColors =
            [
              ( RTFColor
                  { red = Nothing
                  , green = Nothing
                  , blue = Nothing
                  }
              , Nothing
              )
            ,
              ( RTFColor
                  { red = Just 255
                  , green = Just 255
                  , blue = Just 255
                  }
              , Nothing
              )
            ,
              ( RTFColor
                  { red = Just 0
                  , green = Just 0
                  , blue = Just 0
                  }
              , Just (CSSRGB 0 0 0 Nothing)
              )
            ,
              ( RTFColor
                  { red = Just 255
                  , green = Just 255
                  , blue = Just 255
                  }
              , Just (CSGray 100000)
              )
            ,
              ( RTFColor
                  { red = Just 230
                  , green = Just 230
                  , blue = Just 230
                  }
              , Just (CSSRGB 1 2 3 Nothing)
              )
            ]
        }
    )
    [ ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
    , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
    , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
    , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
    , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "cb" (RTFControlParam 3)
    , ContentText " "
    , ContentControlWord NoPrefix "expnd" (RTFControlParam 0)
    , ContentControlWord NoPrefix "expndtw" (RTFControlParam 0)
    , ContentControlWord NoPrefix "kerning" (RTFControlParam 0)
    , ContentText "Normbl text"
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "f" (RTFControlParam 1)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "cb" (RTFControlParam 4)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentText "Code block"
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlWord NoPrefix "cb" (RTFControlParam 1)
    , ContentText " "
    , ContentControlWord NoPrefix "kerning" (RTFControlParam 1)
    , ContentControlWord NoPrefix "expnd" (RTFControlParam 0)
    , ContentControlWord NoPrefix "expndtw" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 2)
    , ContentControlWord NoPrefix "b" SpaceSuffix
    , ContentText "Bold text\n"
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentControlWord NoPrefix "b" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "ul" SpaceSuffix
    , ContentText "Underline text"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "ulnone" SpaceSuffix
    , ContentControlWord NoPrefix "strike" SpaceSuffix
    , ContentControlWord NoPrefix "strikec" (RTFControlParam 0)
    , ContentText " Strikethrough"
    , ContentControlWord NoPrefix "strike" (RTFControlParam 0)
    , ContentControlWord NoPrefix "striked" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "fs" (RTFControlParam 96)
    , ContentText " size 48"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "fs" (RTFControlParam 24)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "qj" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " justify"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " left blign"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "qc" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " center blign"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "qr" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " right blign\n"
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 566)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1133)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1700)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2267)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 2834)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3401)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 3968)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 4535)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5102)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 5669)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6236)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 6803)
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    ]

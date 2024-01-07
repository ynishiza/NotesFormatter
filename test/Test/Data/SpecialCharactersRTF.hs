{-# LANGUAGE QuasiQuotes #-}

module Test.Data.SpecialCharactersRTF (
  content,
  contentText,
) where

import Notes.RTFDoc

contentText :: Text
contentText =
  [multiline|
{\rtf1\ansi\ansicpg1252\cocoartf2639\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset128 HiraginoSans-W3;\f1\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}

{\info{\author Yui Nishizawa}}\vieww11520\viewh8400\viewkind0\deftab720\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\f1\fs28 chbrset128 (Shift JIS) \
\f0\fs28 \cf0 \
\'93\'fa\'96\'7b\'8c\'ea
\f1 \
\f0 AAA\'82\'a2\'82\'a4\'82\'a6\'82\'a8\
\'82\'a9\'82\'ab\'82\'ad\'82\'af\'82\'b1\
\
chbrset0 (ASCII) \\'85\\'b5 = dotdotdot yen\
\f1 ...Yen
\
chbrset128 (Shift JIS) \\'85\\'b5 = VII (Rombn 7) \
\f0 \'85\'a5
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
                    , fontFamily = FNil
                    , fontCharset = Just 128
                    , fontName = "HiraginoSans-W3"
                    }
              , Just
                  FontInfo
                    { fontNum = 1
                    , fontFamily = FRoman
                    , fontCharset = Just 0
                    , fontName = "TimesNewRomanPSMT"
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
            ]
        }
    )
    [ ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
    , ContentControlWord NoPrefix "vieww" (RTFControlParam 11520)
    , ContentControlWord NoPrefix "viewh" (RTFControlParam 8400)
    , ContentControlWord NoPrefix "viewkind" (RTFControlParam 0)
    , ContentControlWord NoPrefix "deftab" (RTFControlParam 720)
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
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " chbrset128 (Shift JIS) "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentEscapedSequence 147
    , ContentEscapedSequence 250
    , ContentEscapedSequence 150
    , ContentEscapedSequence 123
    , ContentEscapedSequence 140
    , ContentEscapedSequence 234
    , ContentText "\n"
    , ContentControlWord NoPrefix "f" (RTFControlParam 1)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentText "AAA"
    , ContentEscapedSequence 130
    , ContentEscapedSequence 162
    , ContentEscapedSequence 130
    , ContentEscapedSequence 164
    , ContentEscapedSequence 130
    , ContentEscapedSequence 166
    , ContentEscapedSequence 130
    , ContentEscapedSequence 168
    , ContentControlSymbol '\n'
    , ContentEscapedSequence 130
    , ContentEscapedSequence 169
    , ContentEscapedSequence 130
    , ContentEscapedSequence 171
    , ContentEscapedSequence 130
    , ContentEscapedSequence 173
    , ContentEscapedSequence 130
    , ContentEscapedSequence 175
    , ContentEscapedSequence 130
    , ContentEscapedSequence 177
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentText "chbrset0 (ASCII) "
    , ContentControlSymbol '\\'
    , ContentText "'85"
    , ContentControlSymbol '\\'
    , ContentText "'b5 = dotdotdot yen"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 1)
    , ContentText " "
    , ContentText "...Yen"
    , ContentText "\n"
    , ContentControlSymbol '\n'
    , ContentText "chbrset128 (Shift JIS) "
    , ContentControlSymbol '\\'
    , ContentText "'85"
    , ContentControlSymbol '\\'
    , ContentText "'b5 = VII (Rombn 7) "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentEscapedSequence 133
    , ContentEscapedSequence 165
    , ContentText "\n"
    ]

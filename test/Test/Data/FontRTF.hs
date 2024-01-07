{-# LANGUAGE QuasiQuotes #-}

module Test.Data.FontRTF (
  content,
  contentText,
) where

import Notes.RTFDoc

contentText :: Text
contentText =
  [multiline|
{\rtf1\ansi\ansicpg1252\cocoartf2639\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\froman\fcharset0 TimesNewRomanPSMT;\f1\fswiss\fcharset0 ArialMT;\f2\fnil\fcharset0 ComicSansMS;\f3\fmodern\fcharset0 CourierNewPSMT;\f4\fswiss\fcharset0 Helvetica;\f5\froman\fcharset0 Times-Roman;\f6\froman\fcharset0 TimesNewRomanPSMT;\f7\fnil\fcharset0 Verdana;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}

{\info{\author Yui Nishizawa}}\vieww11520\viewh8400\viewkind0\deftab720\pard\pardeftab720\qc\partightenfactor0\f0\fs28 \cf0 \ul \ulc0 \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\f1\fs48 \cf0 \ulnone Aribl \
\f2 Comic sbns
\f0 \
\f3 Courier New\
\f4 Helveticb\
\f0 Helveticb Neue\
\f5 Times
\f0 \
\f6 Times New Rombn\
\f7 Verdbnb
\f0 \
\
\
Andble Mono\
Monbco}
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
                    , fontFamily = FSwiss
                    , fontCharset = Just 0
                    , fontName = "ArialMT"
                    }
              , Just
                  FontInfo
                    { fontNum = 2
                    , fontFamily = FNil
                    , fontCharset = Just 0
                    , fontName = "ComicSansMS"
                    }
              , Just
                  FontInfo
                    { fontNum = 3
                    , fontFamily = FModern
                    , fontCharset = Just 0
                    , fontName = "CourierNewPSMT"
                    }
              , Just
                  FontInfo
                    { fontNum = 4
                    , fontFamily = FSwiss
                    , fontCharset = Just 0
                    , fontName = "Helvetica"
                    }
              , Just
                  FontInfo
                    { fontNum = 5
                    , fontFamily = FRoman
                    , fontCharset = Just 0
                    , fontName = "Times-Roman"
                    }
              , Just
                  FontInfo
                    { fontNum = 6
                    , fontFamily = FRoman
                    , fontCharset = Just 0
                    , fontName = "TimesNewRomanPSMT"
                    }
              , Just
                  FontInfo
                    { fontNum = 7
                    , fontFamily = FNil
                    , fontCharset = Just 0
                    , fontName = "Verdana"
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
    , ContentControlWord NoPrefix "pardeftab" (RTFControlParam 720)
    , ContentControlWord NoPrefix "qc" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlWord NoPrefix "ul" SpaceSuffix
    , ContentControlWord NoPrefix "ulc" (RTFControlParam 0)
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
    , ContentControlWord NoPrefix "f" (RTFControlParam 1)
    , ContentControlWord NoPrefix "fs" (RTFControlParam 48)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlWord NoPrefix "ulnone" SpaceSuffix
    , ContentText "Aribl "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 2)
    , ContentText " Comic sbns\n"
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 3)
    , ContentText " Courier New"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 4)
    , ContentText " Helveticb"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " Helveticb Neue"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 5)
    , ContentText " Times\n"
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 6)
    , ContentText " Times New Rombn"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "f" (RTFControlParam 7)
    , ContentText " Verdbnb\n"
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentControlSymbol '\n'
    , ContentText "Andble Mono"
    , ContentControlSymbol '\n'
    , ContentText "Monbco"
    ]

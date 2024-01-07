{-# LANGUAGE QuasiQuotes #-}

module Test.Data.TableRTF (
  content,
  contentText,
) where

import Notes.RTFDoc

contentText :: Text
contentText =
  [multiline|
{\rtf1\ansi\ansicpg1252\cocoartf2639\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset128 HiraginoSans-W3;\f1\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;\red191\green191\blue191;}
{\*\expandedcolortbl;;\csgray\c79525;}

{\*\listtable{\list\listtemplateid1\listhybrid{\listlevel\levelnfc23\levelnfcn23\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{disc\}}{\leveltext\leveltemplateid1\'01\uc0\u8226 ;}{\levelnumbers;}\fi-360\li720\lin720 }{\listlevel\levelnfc23\levelnfcn23\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{hyphen\}}{\leveltext\leveltemplateid2\'01\uc0\u8259 ;}{\levelnumbers;}\fi-360\li1440\lin1440 }{\listname ;}\listid1}}{\*\listoverridetable{\listoverride\listid1\listoverridecount0\ls1}}{\info{\author Yui Nishizawa}}\vieww11520\viewh8400\viewkind0\deftab720\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\f0\fs28 \cf0 \
tbble\
\itap1\trowd \taflags1 \trgaph108\trleft-108 \trbrdrt\brdrnil \trbrdrl\brdrnil \trbrdrr\brdrnil \clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx2880\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx5760\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx8640\pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\f1 \cf0 \cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 Column A\cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 Column B\cell \row\itap1\trowd \taflags1 \trgaph108\trleft-108 \trbrdrl\brdrnil \trbrdrr\brdrnil \clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx2880\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx5760\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx8640\pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 Row 1\cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 1\cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 2\cell \row\itap1\trowd \taflags1 \trgaph108\trleft-108 \trbrdrl\brdrnil \trbrdrt\brdrnil \trbrdrr\brdrnil \clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx2880\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx5760\clvertalc \clshdrawnil \clbrdrt\brdrs\brdrw20\brdrcf2 \clbrdrl\brdrs\brdrw20\brdrcf2 \clbrdrb\brdrs\brdrw20\brdrcf2 \clbrdrr\brdrs\brdrw20\brdrcf2 \clpadl100 \clpadr100 \gaph\cellx8640\pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 Row 2\cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 3\cell \pard\intbl\itap1\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 4\cell \lastrow\row\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\slleading24\pardirnatural\partightenfactor0\cf0 \
list\
\pard\tx220\tx720\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\li720\fi-720\slleading24\pardirnatural\partightenfactor0\ls1\ilvl0\cf0 {\listtext	\uc0\u8226 	}item1\
\pard\tx940\tx1440\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\li1440\fi-1440\slleading24\pardirnatural\partightenfactor0\ls1\ilvl1\cf0 {\listtext	\uc0\u8259 	}subitem 1b\
{\listtext	\uc0\u8259 	}\
\pard\tx220\tx720\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\li720\fi-720\slleading24\pardirnatural\partightenfactor0\ls1\ilvl0\cf0 {\listtext	\uc0\u8226 	}item2\
\pard\tx940\tx1440\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardeftab720\li1440\fi-1440\slleading24\pardirnatural\partightenfactor0\ls1\ilvl1\cf0 {\listtext	\uc0\u8259 	}subitem 2b\
{\listtext	\uc0\u8259 	}\
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
            ,
              ( RTFColor
                  { red = Just 191
                  , green = Just 191
                  , blue = Just 191
                  }
              , Just (CSGray 79525)
              )
            ]
        }
    )
    [ ContentGroup
        [ContentControlWord StarPrefix "listtable" NoSuffix, ContentGroup [ContentControlWord NoPrefix "list" NoSuffix, ContentControlWord NoPrefix "listtemplateid" (RTFControlParam 1), ContentControlWord NoPrefix "listhybrid" NoSuffix, ContentGroup [ContentControlWord NoPrefix "listlevel" NoSuffix, ContentControlWord NoPrefix "levelnfc" (RTFControlParam 23), ContentControlWord NoPrefix "levelnfcn" (RTFControlParam 23), ContentControlWord NoPrefix "leveljc" (RTFControlParam 0), ContentControlWord NoPrefix "leveljcn" (RTFControlParam 0), ContentControlWord NoPrefix "levelfollow" (RTFControlParam 0), ContentControlWord NoPrefix "levelstartat" (RTFControlParam 1), ContentControlWord NoPrefix "levelspace" (RTFControlParam 360), ContentControlWord NoPrefix "levelindent" (RTFControlParam 0), ContentGroup [ContentControlWord StarPrefix "levelmarker" SpaceSuffix, ContentControlSymbol '{', ContentText "disc", ContentControlSymbol '}'], ContentGroup [ContentControlWord NoPrefix "leveltext" NoSuffix, ContentControlWord NoPrefix "leveltemplateid" (RTFControlParam 1), ContentEscapedSequence 1, ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " ;"], ContentGroup [ContentControlWord NoPrefix "levelnumbers" NoSuffix, ContentText ";"], ContentControlWord NoPrefix "fi" (RTFControlParam (-360)), ContentControlWord NoPrefix "li" (RTFControlParam 720), ContentControlWord NoPrefix "lin" (RTFControlParam 720), ContentText " "], ContentGroup [ContentControlWord NoPrefix "listlevel" NoSuffix, ContentControlWord NoPrefix "levelnfc" (RTFControlParam 23), ContentControlWord NoPrefix "levelnfcn" (RTFControlParam 23), ContentControlWord NoPrefix "leveljc" (RTFControlParam 0), ContentControlWord NoPrefix "leveljcn" (RTFControlParam 0), ContentControlWord NoPrefix "levelfollow" (RTFControlParam 0), ContentControlWord NoPrefix "levelstartat" (RTFControlParam 1), ContentControlWord NoPrefix "levelspace" (RTFControlParam 360), ContentControlWord NoPrefix "levelindent" (RTFControlParam 0), ContentGroup [ContentControlWord StarPrefix "levelmarker" SpaceSuffix, ContentControlSymbol '{', ContentText "hyphen", ContentControlSymbol '}'], ContentGroup [ContentControlWord NoPrefix "leveltext" NoSuffix, ContentControlWord NoPrefix "leveltemplateid" (RTFControlParam 2), ContentEscapedSequence 1, ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " ;"], ContentGroup [ContentControlWord NoPrefix "levelnumbers" NoSuffix, ContentText ";"], ContentControlWord NoPrefix "fi" (RTFControlParam (-360)), ContentControlWord NoPrefix "li" (RTFControlParam 1440), ContentControlWord NoPrefix "lin" (RTFControlParam 1440), ContentText " "], ContentGroup [ContentControlWord NoPrefix "listname" SpaceSuffix, ContentText ";"], ContentControlWord NoPrefix "listid" (RTFControlParam 1)]]
    , ContentGroup [ContentControlWord StarPrefix "listoverridetable" NoSuffix, ContentGroup [ContentControlWord NoPrefix "listoverride" NoSuffix, ContentControlWord NoPrefix "listid" (RTFControlParam 1), ContentControlWord NoPrefix "listoverridecount" (RTFControlParam 0), ContentControlWord NoPrefix "ls" (RTFControlParam 1)]]
    , ContentGroup [ContentControlWord NoPrefix "info" NoSuffix, ContentGroup [ContentControlWord NoPrefix "author" SpaceSuffix, ContentText "Yui Nishizawa"]]
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
    , ContentControlWord NoPrefix "f" (RTFControlParam 0)
    , ContentControlWord NoPrefix "fs" (RTFControlParam 28)
    , ContentText " "
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlSymbol '\n'
    , ContentText "tbble"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
    , ContentControlWord NoPrefix "trowd" SpaceSuffix
    , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
    , ContentText " "
    , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
    , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
    , ContentText " "
    , ContentControlWord NoPrefix "trbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "trbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "trbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " Column A"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " Column B"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "row" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
    , ContentControlWord NoPrefix "trowd" SpaceSuffix
    , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
    , ContentText " "
    , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
    , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
    , ContentText " "
    , ContentControlWord NoPrefix "trbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "trbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " Row 1"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " 1"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " 2"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "row" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
    , ContentControlWord NoPrefix "trowd" SpaceSuffix
    , ContentControlWord NoPrefix "taflags" (RTFControlParam 1)
    , ContentText " "
    , ContentControlWord NoPrefix "trgaph" (RTFControlParam 108)
    , ContentControlWord NoPrefix "trleft" (RTFControlParam (-108))
    , ContentText " "
    , ContentControlWord NoPrefix "trbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "trbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "trbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrnil" SpaceSuffix
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 2880)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 5760)
    , ContentControlWord NoPrefix "clvertalc" SpaceSuffix
    , ContentControlWord NoPrefix "clshdrawnil" SpaceSuffix
    , ContentControlWord NoPrefix "clbrdrt" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrl" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrb" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clbrdrr" NoSuffix
    , ContentControlWord NoPrefix "brdrs" NoSuffix
    , ContentControlWord NoPrefix "brdrw" (RTFControlParam 20)
    , ContentControlWord NoPrefix "brdrcf" (RTFControlParam 2)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadl" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "clpadr" (RTFControlParam 100)
    , ContentText " "
    , ContentControlWord NoPrefix "gaph" NoSuffix
    , ContentControlWord NoPrefix "cellx" (RTFControlParam 8640)
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " Row 2"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " 3"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "intbl" NoSuffix
    , ContentControlWord NoPrefix "itap" (RTFControlParam 1)
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
    , ContentText " 4"
    , ContentControlWord NoPrefix "cell" SpaceSuffix
    , ContentControlWord NoPrefix "lastrow" NoSuffix
    , ContentControlWord NoPrefix "row" NoSuffix
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
    , ContentText "list"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 220)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 720)
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
    , ContentControlWord NoPrefix "li" (RTFControlParam 720)
    , ContentControlWord NoPrefix "fi" (RTFControlParam (-720))
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
    , ContentControlWord NoPrefix "ilvl" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " \t"]
    , ContentText "item1"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 940)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1440)
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
    , ContentControlWord NoPrefix "li" (RTFControlParam 1440)
    , ContentControlWord NoPrefix "fi" (RTFControlParam (-1440))
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
    , ContentControlWord NoPrefix "ilvl" (RTFControlParam 1)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
    , ContentText "subitem 1b"
    , ContentControlSymbol '\n'
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 220)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 720)
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
    , ContentControlWord NoPrefix "li" (RTFControlParam 720)
    , ContentControlWord NoPrefix "fi" (RTFControlParam (-720))
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
    , ContentControlWord NoPrefix "ilvl" (RTFControlParam 0)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8226), ContentText " \t"]
    , ContentText "item2"
    , ContentControlSymbol '\n'
    , ContentControlWord NoPrefix "pard" NoSuffix
    , ContentControlWord NoPrefix "tx" (RTFControlParam 940)
    , ContentControlWord NoPrefix "tx" (RTFControlParam 1440)
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
    , ContentControlWord NoPrefix "li" (RTFControlParam 1440)
    , ContentControlWord NoPrefix "fi" (RTFControlParam (-1440))
    , ContentControlWord NoPrefix "slleading" (RTFControlParam 24)
    , ContentControlWord NoPrefix "pardirnatural" NoSuffix
    , ContentControlWord NoPrefix "partightenfactor" (RTFControlParam 0)
    , ContentControlWord NoPrefix "ls" (RTFControlParam 1)
    , ContentControlWord NoPrefix "ilvl" (RTFControlParam 1)
    , ContentControlWord NoPrefix "cf" (RTFControlParam 0)
    , ContentText " "
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
    , ContentText "subitem 2b"
    , ContentControlSymbol '\n'
    , ContentGroup [ContentControlWord NoPrefix "listtext" NoSuffix, ContentText "\t", ContentControlWord NoPrefix "uc" (RTFControlParam 0), ContentControlWord NoPrefix "u" (RTFControlParam 8259), ContentText " \t"]
    , ContentControlSymbol '\n'
    ]

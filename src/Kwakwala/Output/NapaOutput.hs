{-# LANGUAGE OverloadedStrings #-}

-- Technically, this is not the standard
-- NAPA orthography; it's actually the
-- "Southern" orthography as seen
-- at http://www.languagegeek.com/wakashan/kwakwala.html

module Kwakwala.Output.NapaOutput
    ( decodeToNapa
    , decodeToNAPA
    , decodeToNapa2
    , decodeToNAPA2
    , KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    ) where
-- asdfzxcv

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

import Control.Monad
-- import Control.Applicative

-- import Data.Functor
-- import Data.List
import Data.Char

import Kwakwala.Sounds

-- import Data.Either

import System.IO

fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

-------------------------------------------
-- Using Standard Strict Text

-- Seems to use same apostrophe
-- (U+0313) as U'mista.

outputNAPA :: KwakLetter -> T.Text
outputNAPA M   = "m"
outputNAPA MY  = "m\x313"
outputNAPA N   = "n"
outputNAPA NY  = "n\x313"
outputNAPA P   = "p"
outputNAPA T   = "t"
outputNAPA B   = "b"
outputNAPA D   = "d"
outputNAPA PY  = "p\x313"
outputNAPA TY  = "t\x313"
outputNAPA TS  = "c"
outputNAPA TL  = "ƛ"
outputNAPA DZ  = "dᶻ"
outputNAPA DL  = "λ"
outputNAPA TSY = "c\x313"
outputNAPA TLY = "ƛ\x313"
outputNAPA S   = "s"
outputNAPA LH  = "ł"
outputNAPA L   = "l"
outputNAPA LY  = "l\x313"
outputNAPA J   = "y"
outputNAPA JY  = "y\x313"
outputNAPA K   = "k"
outputNAPA KW  = "kʷ"
outputNAPA G   = "g"
outputNAPA GW  = "gʷ"
outputNAPA KY  = "k\x313"
outputNAPA KWY = "k\x313ʷ"
outputNAPA Q   = "q"
outputNAPA QW  = "qʷ"
outputNAPA GU  = "ǧ"
outputNAPA GUW = "ǧʷ"
outputNAPA QY  = "q\x313"
outputNAPA QWY = "q\x313ʷ"
outputNAPA X   = "x"
outputNAPA XW  = "xʷ"
outputNAPA XU  = "x\x30c"
outputNAPA XUW = "x\x30cʷ"
outputNAPA W   = "w"
outputNAPA WY  = "w\x313"
outputNAPA Y   = "ʔ"
outputNAPA H   = "h"
outputNAPA A   = "a"
outputNAPA E   = "e"
outputNAPA I   = "i"
outputNAPA O   = "o"
outputNAPA U   = "u"
outputNAPA AU  = "ə"
-- outputNAPA Spc  = " "
-- asdfzxcv

outputNAPA' :: KwakLetter -> T.Text
outputNAPA' M   = "M"
outputNAPA' MY  = "M\x313"
outputNAPA' N   = "N"
outputNAPA' NY  = "N\x313"
outputNAPA' P   = "P"
outputNAPA' T   = "T"
outputNAPA' B   = "B"
outputNAPA' D   = "D"
outputNAPA' PY  = "P\x313"
outputNAPA' TY  = "T\x313"
outputNAPA' TS  = "C"
outputNAPA' TL  = "ƛ"
outputNAPA' DZ  = "Dᶻ"
outputNAPA' DL  = "Λ"
outputNAPA' TSY = "C\x313"
outputNAPA' TLY = "ƛ̓"
outputNAPA' S   = "S"
outputNAPA' LH  = "Ł"
outputNAPA' L   = "L"
outputNAPA' LY  = "L\x313"
outputNAPA' J   = "Y"
outputNAPA' JY  = "Y\x313"
outputNAPA' K   = "K"
outputNAPA' KW  = "Kʷ"
outputNAPA' G   = "G"
outputNAPA' GW  = "Gʷ"
outputNAPA' KY  = "K\x313"
outputNAPA' KWY = "K\x313ʷ"
outputNAPA' Q   = "Q"
outputNAPA' QW  = "Qʷ"
outputNAPA' GU  = "Ǧ"
outputNAPA' GUW = "Ǧʷ"
outputNAPA' QY  = "Q\x313"
outputNAPA' QWY = "Q\x313ʷ"
outputNAPA' X   = "X"
outputNAPA' XW  = "Xʷ"
outputNAPA' XU  = "X\x30c"
outputNAPA' XUW = "X\x30cʷ"
outputNAPA' W   = "W"
outputNAPA' WY  = "W\x313"
outputNAPA' Y   = "ʔ"
outputNAPA' H   = "H"
outputNAPA' A   = "A"
outputNAPA' E   = "E"
outputNAPA' I   = "I"
outputNAPA' O   = "O"
outputNAPA' U   = "U"
outputNAPA' AU  = "Ə"
-- asdfzxcv

-- Strict Text-based output
decodeToNapa :: [CasedChar] -> T.Text
decodeToNapa = T.concat . (map $ mapChar $ mapCase outputNAPA' outputNAPA)

decodeToNAPA :: [CasedChar] -> T.Text
decodeToNAPA = decodeToNapa

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputNAPA2 :: KwakLetter -> TL.Builder
outputNAPA2 M   = "m"
outputNAPA2 MY  = "m\x313"
outputNAPA2 N   = "n"
outputNAPA2 NY  = "n\x313"
outputNAPA2 P   = "p"
outputNAPA2 T   = "t"
outputNAPA2 B   = "b"
outputNAPA2 D   = "d"
outputNAPA2 PY  = "p\x313"
outputNAPA2 TY  = "t\x313"
outputNAPA2 TS  = "c"
outputNAPA2 TL  = "ƛ"
outputNAPA2 DZ  = "dᶻ"
outputNAPA2 DL  = "λ"
outputNAPA2 TSY = "c\x313"
outputNAPA2 TLY = "ƛ\x313"
outputNAPA2 S   = "s"
outputNAPA2 LH  = "ł"
outputNAPA2 L   = "l"
outputNAPA2 LY  = "l\x313"
outputNAPA2 J   = "y"
outputNAPA2 JY  = "y\x313"
outputNAPA2 K   = "k"
outputNAPA2 KW  = "kʷ"
outputNAPA2 G   = "g"
outputNAPA2 GW  = "gʷ"
outputNAPA2 KY  = "k\x313"
outputNAPA2 KWY = "k\x313ʷ"
outputNAPA2 Q   = "q"
outputNAPA2 QW  = "qʷ"
outputNAPA2 GU  = "ǧ"
outputNAPA2 GUW = "ǧʷ"
outputNAPA2 QY  = "q\x313"
outputNAPA2 QWY = "q\x313ʷ"
outputNAPA2 X   = "x"
outputNAPA2 XW  = "xʷ"
outputNAPA2 XU  = "x\x30c"
outputNAPA2 XUW = "x\x30cʷ"
outputNAPA2 W   = "w"
outputNAPA2 WY  = "w\x313"
outputNAPA2 Y   = "ʔ"
outputNAPA2 H   = "h"
outputNAPA2 A   = "a"
outputNAPA2 E   = "e"
outputNAPA2 I   = "i"
outputNAPA2 O   = "o"
outputNAPA2 U   = "u"
outputNAPA2 AU  = "ə"
-- asdfzxcv

-- Builder-based Upper-case letters
outputNAPA2' :: KwakLetter -> TL.Builder
outputNAPA2' M   = "M"
outputNAPA2' MY  = "M\x313"
outputNAPA2' N   = "N"
outputNAPA2' NY  = "N\x313"
outputNAPA2' P   = "P"
outputNAPA2' T   = "T"
outputNAPA2' B   = "B"
outputNAPA2' D   = "D"
outputNAPA2' PY  = "P\x313"
outputNAPA2' TY  = "T\x313"
outputNAPA2' TS  = "C"
outputNAPA2' TL  = "ƛ"
outputNAPA2' DZ  = "Dᶻ"
outputNAPA2' DL  = "Λ"
outputNAPA2' TSY = "C\x313"
outputNAPA2' TLY = "ƛ̓"
outputNAPA2' S   = "S"
outputNAPA2' LH  = "Ł"
outputNAPA2' L   = "L"
outputNAPA2' LY  = "L\x313"
outputNAPA2' J   = "Y"
outputNAPA2' JY  = "Y\x313"
outputNAPA2' K   = "K"
outputNAPA2' KW  = "Kʷ"
outputNAPA2' G   = "G"
outputNAPA2' GW  = "Gʷ"
outputNAPA2' KY  = "K\x313"
outputNAPA2' KWY = "K\x313ʷ"
outputNAPA2' Q   = "Q"
outputNAPA2' QW  = "Qʷ"
outputNAPA2' GU  = "Ǧ"
outputNAPA2' GUW = "Ǧʷ"
outputNAPA2' QY  = "Q\x313"
outputNAPA2' QWY = "Q\x313ʷ"
outputNAPA2' X   = "X"
outputNAPA2' XW  = "Xʷ"
outputNAPA2' XU  = "X\x30c"
outputNAPA2' XUW = "X\x30cʷ"
outputNAPA2' W   = "W"
outputNAPA2' WY  = "W\x313"
outputNAPA2' Y   = "ʔ"
outputNAPA2' H   = "H"
outputNAPA2' A   = "A"
outputNAPA2' E   = "E"
outputNAPA2' I   = "I"
outputNAPA2' O   = "O"
outputNAPA2' U   = "U"
outputNAPA2' AU  = "Ə"
-- asdfzxcv


decodeToNapa2 :: [CasedChar] -> T.Text
decodeToNapa2 = TL.toStrict . decodeToNapaLazy -- TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

decodeToNAPA2 :: [CasedChar] -> T.Text
decodeToNAPA2 = decodeToNapa2

decodeToNapaLazy :: [CasedChar] -> TL.Text
decodeToNapaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

decodeToNAPALazy :: [CasedChar] -> TL.Text
decodeToNAPALazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))



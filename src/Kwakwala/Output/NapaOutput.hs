{-|
Module      : Kwakwala.Output.NapaOutput
Description : Output for the usual Kwak'wala NAPA orthography
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains output functions for
the variant of NAPA that is most commonly
used for Kwak'wala. It's also known as the
\"Southern\" Orthography, as can be seen at
<http://www.languagegeek.com/wakashan/kwakwala.html>

-}

module Kwakwala.Output.NapaOutput
    -- * Exclusively Using Strict Text
    ( decodeToNapa
    , decodeToNAPA
    -- * Strict Text with Builders
    , decodeToNapa2
    , decodeToNAPA2
    -- * Lazy Text Output
    , decodeToNapaLazy
    , decodeToNAPALazy
    -- * Configurable
    , decodeToNapaC
    , decodeToNapaLazyC
    -- * Configuration
    , NapaOutputConfig(..)
    ) where

import Data.Text              qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Kwakwala.Sounds

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

-- | Output `T.Text` in the Kwak'wala variant of NAPA.
--
-- This version uses strict `T.Text` output.
decodeToNapa :: [CasedChar] -> T.Text
decodeToNapa = T.concat . (map $ mapChar $ mapCase outputNAPA' outputNAPA)

-- | Synonym for `decodeToNapa`.
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

--------------------------------------------
-- Using Custom Options

data NapaOutputConfig = NapaOutputConfig
  { nocUseUpperLambda :: Bool
  } deriving (Show, Eq)


-- Builder-based lower-case letter output
outputNAPAC :: NapaOutputConfig -> KwakLetter -> TL.Builder
outputNAPAC _ M   = "m"
outputNAPAC _ MY  = "m\x313"
outputNAPAC _ N   = "n"
outputNAPAC _ NY  = "n\x313"
outputNAPAC _ P   = "p"
outputNAPAC _ T   = "t"
outputNAPAC _ B   = "b"
outputNAPAC _ D   = "d"
outputNAPAC _ PY  = "p\x313"
outputNAPAC _ TY  = "t\x313"
outputNAPAC _ TS  = "c"
outputNAPAC _ TL  = "ƛ"
outputNAPAC _ DZ  = "dᶻ"
outputNAPAC _ DL  = "λ"
outputNAPAC _ TSY = "c\x313"
outputNAPAC _ TLY = "ƛ\x313"
outputNAPAC _ S   = "s"
outputNAPAC _ LH  = "ł"
outputNAPAC _ L   = "l"
outputNAPAC _ LY  = "l\x313"
outputNAPAC _ J   = "y"
outputNAPAC _ JY  = "y\x313"
outputNAPAC _ K   = "k"
outputNAPAC _ KW  = "kʷ"
outputNAPAC _ G   = "g"
outputNAPAC _ GW  = "gʷ"
outputNAPAC _ KY  = "k\x313"
outputNAPAC _ KWY = "k\x313ʷ"
outputNAPAC _ Q   = "q"
outputNAPAC _ QW  = "qʷ"
outputNAPAC _ GU  = "ǧ"
outputNAPAC _ GUW = "ǧʷ"
outputNAPAC _ QY  = "q\x313"
outputNAPAC _ QWY = "q\x313ʷ"
outputNAPAC _ X   = "x"
outputNAPAC _ XW  = "xʷ"
outputNAPAC _ XU  = "x\x30c"
outputNAPAC _ XUW = "x\x30cʷ"
outputNAPAC _ W   = "w"
outputNAPAC _ WY  = "w\x313"
outputNAPAC _ Y   = "ʔ"
outputNAPAC _ H   = "h"
outputNAPAC _ A   = "a"
outputNAPAC _ E   = "e"
outputNAPAC _ I   = "i"
outputNAPAC _ O   = "o"
outputNAPAC _ U   = "u"
outputNAPAC _ AU  = "ə"

-- Builder-based Upper-case letters
outputNAPAC' :: NapaOutputConfig -> KwakLetter -> TL.Builder
outputNAPAC' _ M   = "M"
outputNAPAC' _ MY  = "M\x313"
outputNAPAC' _ N   = "N"
outputNAPAC' _ NY  = "N\x313"
outputNAPAC' _ P   = "P"
outputNAPAC' _ T   = "T"
outputNAPAC' _ B   = "B"
outputNAPAC' _ D   = "D"
outputNAPAC' _ PY  = "P\x313"
outputNAPAC' _ TY  = "T\x313"
outputNAPAC' _ TS  = "C"
outputNAPAC' _ TL  = "ƛ"
outputNAPAC' _ DZ  = "Dᶻ"
outputNAPAC' noc DL
  | (nocUseUpperLambda noc) = "Λ"
  | otherwise = "λ"
outputNAPAC' _ TSY = "C\x313"
outputNAPAC' _ TLY = "ƛ̓"
outputNAPAC' _ S   = "S"
outputNAPAC' _ LH  = "Ł"
outputNAPAC' _ L   = "L"
outputNAPAC' _ LY  = "L\x313"
outputNAPAC' _ J   = "Y"
outputNAPAC' _ JY  = "Y\x313"
outputNAPAC' _ K   = "K"
outputNAPAC' _ KW  = "Kʷ"
outputNAPAC' _ G   = "G"
outputNAPAC' _ GW  = "Gʷ"
outputNAPAC' _ KY  = "K\x313"
outputNAPAC' _ KWY = "K\x313ʷ"
outputNAPAC' _ Q   = "Q"
outputNAPAC' _ QW  = "Qʷ"
outputNAPAC' _ GU  = "Ǧ"
outputNAPAC' _ GUW = "Ǧʷ"
outputNAPAC' _ QY  = "Q\x313"
outputNAPAC' _ QWY = "Q\x313ʷ"
outputNAPAC' _ X   = "X"
outputNAPAC' _ XW  = "Xʷ"
outputNAPAC' _ XU  = "X\x30c"
outputNAPAC' _ XUW = "X\x30cʷ"
outputNAPAC' _ W   = "W"
outputNAPAC' _ WY  = "W\x313"
outputNAPAC' _ Y   = "ʔ"
outputNAPAC' _ H   = "H"
outputNAPAC' _ A   = "A"
outputNAPAC' _ E   = "E"
outputNAPAC' _ I   = "I"
outputNAPAC' _ O   = "O"
outputNAPAC' _ U   = "U"
outputNAPAC' _ AU  = "Ə"


--------------------------------------------
-- Actual Functions to Use

-- | Output `T.Text` in the Kwak'wala variant of NAPA.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToNapa2 :: [CasedChar] -> T.Text
decodeToNapa2 = TL.toStrict . decodeToNapaLazy

-- | Synonym for `decodeToNapa2`.
decodeToNAPA2 :: [CasedChar] -> T.Text
decodeToNAPA2 = decodeToNapa2

-- | Output `TL.Text` in the Kwak'wala variant of NAPA.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToNapaLazy :: [CasedChar] -> TL.Text
decodeToNapaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

-- | Synonym for `decodeToNapaLazy`.
decodeToNAPALazy :: [CasedChar] -> TL.Text
decodeToNAPALazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

-- | Output `T.Text` in the Kwak'wala variant of NAPA,
-- with custom options
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToNapaC :: NapaOutputConfig -> [CasedChar] -> T.Text
decodeToNapaC noc = TL.toStrict . (decodeToNapaLazyC noc)

-- | Output `TL.Text` in the Kwak'wala variant of NAPA,
-- with custom options.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToNapaLazyC :: NapaOutputConfig -> [CasedChar] -> TL.Text
decodeToNapaLazyC noc = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase (outputNAPAC' noc) (outputNAPAC noc)))

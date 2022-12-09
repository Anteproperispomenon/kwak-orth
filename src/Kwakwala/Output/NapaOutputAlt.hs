{-|
Module      : Kwakwala.Output.NapaOutputAlt
Description : Output for the \"standard\" NAPA orthography.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains output functions for
the standard variant of NAPA. This version
is not commonly used for Kwak'wala, but might
be used in some older texts. See
<https://en.wikipedia.org/wiki/Americanist_phonetic_notation>
for details.

-}

module Kwakwala.Output.NapaOutputAlt
    -- * Exclusively Using Strict Text
    ( decodeToNapaAlt
    , decodeToNAPAalt
    -- * Strict Text with Builders
    , decodeToNapaAlt2
    , decodeToNAPAalt2
    -- * Lazy Text Output
    , decodeToNapaAltLazy
    , decodeToNAPAaltLazy
    ) where

import Data.Text              qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Kwakwala.Sounds

-------------------------------------------
-- Using Standard Strict Text

outputNAPAalt :: KwakLetter -> T.Text
outputNAPAalt M   = "m"
outputNAPAalt MY  = "m̓"
outputNAPAalt N   = "n"
outputNAPAalt NY  = "n̓"
outputNAPAalt P   = "p"
outputNAPAalt T   = "t"
outputNAPAalt B   = "b"
outputNAPAalt D   = "d"
outputNAPAalt PY  = "p̓"
outputNAPAalt TY  = "t̓"
outputNAPAalt TS  = "c"
outputNAPAalt TL  = "ƛ"
outputNAPAalt DZ  = "ʒ"
outputNAPAalt DL  = "λ"
outputNAPAalt TSY = "c̓"
outputNAPAalt TLY = "ƛ̓"
outputNAPAalt S   = "s"
outputNAPAalt LH  = "ł"
outputNAPAalt L   = "l"
outputNAPAalt LY  = "l̓"
outputNAPAalt J   = "y"
outputNAPAalt JY  = "y̓"
outputNAPAalt K   = "k"
outputNAPAalt KW  = "kʷ"
outputNAPAalt G   = "g"
outputNAPAalt GW  = "gʷ"
outputNAPAalt KY  = "k̓"
outputNAPAalt KWY = "k̓ʷ"
outputNAPAalt Q   = "q"
outputNAPAalt QW  = "qʷ"
outputNAPAalt GU  = "ġ"
outputNAPAalt GUW = "ġʷ"
outputNAPAalt QY  = "q̓"
outputNAPAalt QWY = "q̓ʷ"
outputNAPAalt X   = "x"
outputNAPAalt XW  = "xʷ"
outputNAPAalt XU  = "x̣"
outputNAPAalt XUW = "x̣ʷ"
outputNAPAalt W   = "w"
outputNAPAalt WY  = "w̓"
outputNAPAalt Y   = "ʔ"
outputNAPAalt H   = "h"
outputNAPAalt A   = "a"
outputNAPAalt E   = "e"
outputNAPAalt I   = "i"
outputNAPAalt O   = "o"
outputNAPAalt U   = "u"
outputNAPAalt AU  = "ə"

outputNAPAalt' :: KwakLetter -> T.Text
outputNAPAalt' M   = "M"
outputNAPAalt' MY  = "M̓"
outputNAPAalt' N   = "N"
outputNAPAalt' NY  = "N̓"
outputNAPAalt' P   = "P"
outputNAPAalt' T   = "T"
outputNAPAalt' B   = "B"
outputNAPAalt' D   = "D"
outputNAPAalt' PY  = "P̓"
outputNAPAalt' TY  = "T̓"
outputNAPAalt' TS  = "C"
outputNAPAalt' TL  = "ƛ"
outputNAPAalt' DZ  = "Ʒ"
outputNAPAalt' DL  = "Λ"
outputNAPAalt' TSY = "C̓"
outputNAPAalt' TLY = "ƛ̓"
outputNAPAalt' S   = "S"
outputNAPAalt' LH  = "ł"
outputNAPAalt' L   = "L"
outputNAPAalt' LY  = "L̓"
outputNAPAalt' J   = "Y"
outputNAPAalt' JY  = "Y̓"
outputNAPAalt' K   = "K"
outputNAPAalt' KW  = "Kʷ"
outputNAPAalt' G   = "G"
outputNAPAalt' GW  = "Gʷ"
outputNAPAalt' KY  = "K̓"
outputNAPAalt' KWY = "K̓ʷ"
outputNAPAalt' Q   = "Q"
outputNAPAalt' QW  = "Qʷ"
outputNAPAalt' GU  = "Ġ"
outputNAPAalt' GUW = "Ġʷ"
outputNAPAalt' QY  = "Q̓"
outputNAPAalt' QWY = "Q̓ʷ"
outputNAPAalt' X   = "X"
outputNAPAalt' XW  = "Xʷ"
outputNAPAalt' XU  = "X̣"
outputNAPAalt' XUW = "X̣ʷ"
outputNAPAalt' W   = "W"
outputNAPAalt' WY  = "W̓"
outputNAPAalt' Y   = "ʔ"
outputNAPAalt' H   = "H"
outputNAPAalt' A   = "A"
outputNAPAalt' E   = "E"
outputNAPAalt' I   = "I"
outputNAPAalt' O   = "O"
outputNAPAalt' U   = "U"
outputNAPAalt' AU  = "Ə"

-- | Output `T.Text` in the \"standard\" variant of NAPA.
--
-- This version uses strict `T.Text` output.
decodeToNapaAlt :: [CasedChar] -> T.Text
decodeToNapaAlt = T.concat . (map $ mapChar $ mapCase outputNAPAalt' outputNAPAalt)

-- | Synonym for `decodeToNapaAlt`.
decodeToNAPAalt :: [CasedChar] -> T.Text
decodeToNAPAalt = decodeToNapaAlt

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputNAPAalt2 :: KwakLetter -> TL.Builder
outputNAPAalt2 M   = "m"
outputNAPAalt2 MY  = "m̓"
outputNAPAalt2 N   = "n"
outputNAPAalt2 NY  = "n̓"
outputNAPAalt2 P   = "p"
outputNAPAalt2 T   = "t"
outputNAPAalt2 B   = "b"
outputNAPAalt2 D   = "d"
outputNAPAalt2 PY  = "p̓"
outputNAPAalt2 TY  = "t̓"
outputNAPAalt2 TS  = "c"
outputNAPAalt2 TL  = "ƛ"
outputNAPAalt2 DZ  = "ʒ"
outputNAPAalt2 DL  = "λ"
outputNAPAalt2 TSY = "c̓"
outputNAPAalt2 TLY = "ƛ̓"
outputNAPAalt2 S   = "s"
outputNAPAalt2 LH  = "ł"
outputNAPAalt2 L   = "l"
outputNAPAalt2 LY  = "l̓"
outputNAPAalt2 J   = "y"
outputNAPAalt2 JY  = "y̓"
outputNAPAalt2 K   = "k"
outputNAPAalt2 KW  = "kʷ"
outputNAPAalt2 G   = "g"
outputNAPAalt2 GW  = "gʷ"
outputNAPAalt2 KY  = "k̓"
outputNAPAalt2 KWY = "k̓ʷ"
outputNAPAalt2 Q   = "q"
outputNAPAalt2 QW  = "qʷ"
outputNAPAalt2 GU  = "ġ"
outputNAPAalt2 GUW = "ġʷ"
outputNAPAalt2 QY  = "q̓"
outputNAPAalt2 QWY = "q̓ʷ"
outputNAPAalt2 X   = "x"
outputNAPAalt2 XW  = "xʷ"
outputNAPAalt2 XU  = "x̣"
outputNAPAalt2 XUW = "x̣ʷ"
outputNAPAalt2 W   = "w"
outputNAPAalt2 WY  = "w̓"
outputNAPAalt2 Y   = "ʔ"
outputNAPAalt2 H   = "h"
outputNAPAalt2 A   = "a"
outputNAPAalt2 E   = "e"
outputNAPAalt2 I   = "i"
outputNAPAalt2 O   = "o"
outputNAPAalt2 U   = "u"
outputNAPAalt2 AU  = "ə"

-- Builder-based Capital Letter Output
outputNAPAalt2' :: KwakLetter -> TL.Builder
outputNAPAalt2' M   = "M"
outputNAPAalt2' MY  = "M̓"
outputNAPAalt2' N   = "N"
outputNAPAalt2' NY  = "N̓"
outputNAPAalt2' P   = "P"
outputNAPAalt2' T   = "T"
outputNAPAalt2' B   = "B"
outputNAPAalt2' D   = "D"
outputNAPAalt2' PY  = "P̓"
outputNAPAalt2' TY  = "T̓"
outputNAPAalt2' TS  = "C"
outputNAPAalt2' TL  = "ƛ"
outputNAPAalt2' DZ  = "Ʒ"
outputNAPAalt2' DL  = "Λ"
outputNAPAalt2' TSY = "C̓"
outputNAPAalt2' TLY = "ƛ̓"
outputNAPAalt2' S   = "S"
outputNAPAalt2' LH  = "ł"
outputNAPAalt2' L   = "L"
outputNAPAalt2' LY  = "L̓"
outputNAPAalt2' J   = "Y"
outputNAPAalt2' JY  = "Y̓"
outputNAPAalt2' K   = "K"
outputNAPAalt2' KW  = "Kʷ"
outputNAPAalt2' G   = "G"
outputNAPAalt2' GW  = "Gʷ"
outputNAPAalt2' KY  = "K̓"
outputNAPAalt2' KWY = "K̓ʷ"
outputNAPAalt2' Q   = "Q"
outputNAPAalt2' QW  = "Qʷ"
outputNAPAalt2' GU  = "Ġ"
outputNAPAalt2' GUW = "Ġʷ"
outputNAPAalt2' QY  = "Q̓"
outputNAPAalt2' QWY = "Q̓ʷ"
outputNAPAalt2' X   = "X"
outputNAPAalt2' XW  = "Xʷ"
outputNAPAalt2' XU  = "X̣"
outputNAPAalt2' XUW = "X̣ʷ"
outputNAPAalt2' W   = "W"
outputNAPAalt2' WY  = "W̓"
outputNAPAalt2' Y   = "ʔ"
outputNAPAalt2' H   = "H"
outputNAPAalt2' A   = "A"
outputNAPAalt2' E   = "E"
outputNAPAalt2' I   = "I"
outputNAPAalt2' O   = "O"
outputNAPAalt2' U   = "U"
outputNAPAalt2' AU  = "Ə"

-- | Output `T.Text` in the \"standard\" variant of NAPA.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToNapaAlt2 :: [CasedChar] -> T.Text
decodeToNapaAlt2 = TL.toStrict . decodeToNapaAltLazy

-- | Synonym for `decodeToNapaAlt2`.
decodeToNAPAalt2 :: [CasedChar] -> T.Text
decodeToNAPAalt2 = decodeToNapaAlt2

-- | Output `TL.Text` in the \"standard\" variant of NAPA.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToNapaAltLazy :: [CasedChar] -> TL.Text
decodeToNapaAltLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPAalt2' outputNAPAalt2))

-- | Synonym for `decodeToNapaAltLazy`.
decodeToNAPAaltLazy :: [CasedChar] -> TL.Text
decodeToNAPAaltLazy = decodeToNapaAltLazy

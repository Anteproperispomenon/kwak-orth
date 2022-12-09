{-|
Module      : Kwakwala.Output.PseudoBoasOutput
Description : Output for a Boas-like orthography.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains output functions for
an orthography that tries to replicate
Boas's orginial orthorgraphy for Kwak'wala.
Since Boas's orthography is much more
context-sensitive than most modern
orthographies, it is more difficult to
write an output generator that correctly
replicates the true orthography.

-}

module Kwakwala.Output.PseudoBoasOutput
    ( decodeToPseudoBoas
    , decodeToPseudoBoas2
    , decodeToPseudoBoasLazy
    ) where

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

import Data.String

import Kwakwala.Sounds

-------------------------------------------
-- For Lower Case

outputPseudoBoasX :: (IsString a) => KwakLetter -> a
outputPseudoBoasX M   = "m"
outputPseudoBoasX MY  = "ᵋm"
outputPseudoBoasX N   = "n"
outputPseudoBoasX NY  = "ᵋn"
outputPseudoBoasX P   = "p"
outputPseudoBoasX T   = "t"
outputPseudoBoasX B   = "b"
outputPseudoBoasX D   = "d"
outputPseudoBoasX PY  = "p!"
outputPseudoBoasX TY  = "t!"
outputPseudoBoasX TS  = "ts"
outputPseudoBoasX TL  = "ʟ"
outputPseudoBoasX DZ  = "dz"
outputPseudoBoasX DL  = "ʟ\x323" -- \x323 = combining dot below
outputPseudoBoasX TSY = "ts!"
outputPseudoBoasX TLY = "ʟ!"
outputPseudoBoasX S   = "s"
outputPseudoBoasX LH  = "ł"
outputPseudoBoasX L   = "l"
outputPseudoBoasX LY  = "ᵋl"
outputPseudoBoasX J   = "y"
outputPseudoBoasX JY  = "ᵋy"
outputPseudoBoasX K   = "k·"
outputPseudoBoasX KW  = "kᵘ"
outputPseudoBoasX G   = "g·"
outputPseudoBoasX GW  = "gᵘ"
outputPseudoBoasX KY  = "k·!"
outputPseudoBoasX KWY = "k!ᵘ"
outputPseudoBoasX Q   = "q"
outputPseudoBoasX QW  = "qᵘ"
outputPseudoBoasX GU  = "g\x323" -- "g̣"
outputPseudoBoasX GUW = "g\x323ᵘ" -- "g̣ᵘ"
outputPseudoBoasX QY  = "q!"
outputPseudoBoasX QWY = "q!ᵘ"
outputPseudoBoasX X   = "x·"
outputPseudoBoasX XW  = "x̣ᵘ"
outputPseudoBoasX XU  = "x"
outputPseudoBoasX XUW = "xᵘ"
outputPseudoBoasX W   = "w"
outputPseudoBoasX WY  = "ᵋw"
outputPseudoBoasX Y   = "ᵋ"
outputPseudoBoasX H   = "h"
outputPseudoBoasX A   = "a" -- a
outputPseudoBoasX E   = "ä" -- a umlaut
outputPseudoBoasX I   = "i" -- i
outputPseudoBoasX O   = "â" -- a hatchet
outputPseudoBoasX U   = "u" -- u
outputPseudoBoasX AU  = "ᴇ"

-------------------------------------------
-- For Upper case

outputPseudoBoasX' :: (IsString a) => KwakLetter -> a
outputPseudoBoasX' M   = "M"
outputPseudoBoasX' MY  = "ᵋM"
outputPseudoBoasX' N   = "N"
outputPseudoBoasX' NY  = "ᵋN"
outputPseudoBoasX' P   = "P"
outputPseudoBoasX' T   = "T"
outputPseudoBoasX' B   = "B"
outputPseudoBoasX' D   = "D"
outputPseudoBoasX' PY  = "P!"
outputPseudoBoasX' TY  = "T!"
outputPseudoBoasX' TS  = "Ts"
outputPseudoBoasX' TL  = "Ⱡ"
outputPseudoBoasX' DZ  = "Dz"
outputPseudoBoasX' DL  = "Ⱡ\x323" -- \x323 = combining dot below -- Ⱡ
outputPseudoBoasX' TSY = "Ts!"
outputPseudoBoasX' TLY = "Ⱡ!"
outputPseudoBoasX' S   = "S"
outputPseudoBoasX' LH  = "Ł"
outputPseudoBoasX' L   = "L"
outputPseudoBoasX' LY  = "ᵋL"
outputPseudoBoasX' J   = "Y"
outputPseudoBoasX' JY  = "ᵋY"
outputPseudoBoasX' K   = "K·"
outputPseudoBoasX' KW  = "Kᵘ"
outputPseudoBoasX' G   = "G·"
outputPseudoBoasX' GW  = "Gᵘ"
outputPseudoBoasX' KY  = "K·!"
outputPseudoBoasX' KWY = "K!ᵘ"
outputPseudoBoasX' Q   = "Q"
outputPseudoBoasX' QW  = "Qᵘ"
outputPseudoBoasX' GU  = "G\x323" -- "g̣"
outputPseudoBoasX' GUW = "G\x323ᵘ" -- "g̣ᵘ"
outputPseudoBoasX' QY  = "Q!"
outputPseudoBoasX' QWY = "Q!ᵘ"
outputPseudoBoasX' X   = "X·"
outputPseudoBoasX' XW  = "X̣ᵘ"
outputPseudoBoasX' XU  = "X"
outputPseudoBoasX' XUW = "Xᵘ"
outputPseudoBoasX' W   = "W"
outputPseudoBoasX' WY  = "ᵋW"
outputPseudoBoasX' Y   = "ᵋ"
outputPseudoBoasX' H   = "H"
outputPseudoBoasX' A   = "A" -- a
outputPseudoBoasX' E   = "Ä" -- a umlaut
outputPseudoBoasX' I   = "I" -- i
outputPseudoBoasX' O   = "Â" -- a hatchet
outputPseudoBoasX' U   = "U" -- u
outputPseudoBoasX' AU  = "Û"

----------------------
-- Possibilities for smallcaps L in uppercase

-- Ļ L-cedilla / L-comma
-- Ŀ L-middot
-- Ɫ L-tilde (also ɫ)
-- Ⱡ L-double stroke

outputPseudoBoas :: KwakLetter -> T.Text
outputPseudoBoas = outputPseudoBoasX

outputPseudoBoas' :: KwakLetter -> T.Text
outputPseudoBoas' = outputPseudoBoasX'

-- | Pseudo-Boas text output.
--
-- This version uses strict `Text` output.
decodeToPseudoBoas :: [CasedChar] -> T.Text
decodeToPseudoBoas = T.concat . (map $ mapChar $ mapCase outputPseudoBoas' outputPseudoBoas)

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputPseudoBoas2 :: KwakLetter -> TL.Builder
outputPseudoBoas2 = outputPseudoBoasX

outputPseudoBoas2' :: KwakLetter -> TL.Builder
outputPseudoBoas2' = outputPseudoBoasX'

-- | Pseudo-Boas text output.
--
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToPseudoBoas2 :: [CasedChar] -> T.Text
decodeToPseudoBoas2 = TL.toStrict . decodeToPseudoBoasLazy

-- | Pseudo-Boas text output.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToPseudoBoasLazy :: [CasedChar] -> TL.Text
decodeToPseudoBoasLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputPseudoBoas2' outputPseudoBoas2))

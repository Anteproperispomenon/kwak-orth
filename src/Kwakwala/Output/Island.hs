{-|
Module      : Kwakwala.Output.Island
Description : Output for the "Island" font.
Copyright   : (c) David Wilson, 2023
License     : BSD-3

This module has output functions not for
a specific orthography, but for the 
"Island" font that uses existing ASCII
codepoints for Kwak'wala NAPA characters.
-}

module Kwakwala.Output.Island
    -- * Exclusively Using Strict Text
    ( decodeToIsland
    -- * Strict Text with Builders
    , decodeToIsland2
    -- * Lazy Text Output
    , decodeToIslandLazy
    ) where

import Data.Text              qualified as T
import Data.Text.IO           qualified as T
import Data.Text.Encoding     qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Control.Monad
import Data.Char
import Data.String

import Kwakwala.Sounds


-------------------------------------------
-- Using Standard Strict Text

-- | Output a lower-case Island character.
outputIslandX :: (IsString a) => KwakLetter -> a
outputIslandX M   = "m"
outputIslandX MY  = "m{"
outputIslandX N   = "n"
outputIslandX NY  = "n{"
outputIslandX P   = "p"
outputIslandX T   = "t"
outputIslandX B   = "b"
outputIslandX D   = "d"
outputIslandX PY  = "p`"
outputIslandX TY  = "t`"
outputIslandX TS  = "c"
outputIslandX TL  = "["
outputIslandX DZ  = "d+"
outputIslandX DL  = "]"
outputIslandX TSY = "c`"
outputIslandX TLY = "[{"
outputIslandX S   = "s"
outputIslandX LH  = ">"
outputIslandX L   = "l"
outputIslandX LY  = "l`" -- "l{"
outputIslandX J   = "y"
outputIslandX JY  = "y`" -- "y{"
outputIslandX K   = "k"
outputIslandX KW  = "k#"
outputIslandX G   = "g"
outputIslandX GW  = "g#"
outputIslandX KY  = "k`"  -- "k{"
outputIslandX KWY = "k`#" -- "k{#"
outputIslandX Q   = "q"
outputIslandX QW  = "q#"
outputIslandX GU  = "g^"
outputIslandX GUW = "g^#"
outputIslandX QY  = "q`"  -- "q{"
outputIslandX QWY = "q`#" -- "q{#"
outputIslandX X   = "x"
outputIslandX XW  = "x#"
outputIslandX XU  = "x^"
outputIslandX XUW = "x^#"
outputIslandX W   = "w"
outputIslandX WY  = "w{"
outputIslandX Y   = "%"
outputIslandX H   = "h"
outputIslandX A   = "a"
outputIslandX E   = "e"
outputIslandX I   = "i"
outputIslandX O   = "o"
outputIslandX U   = "u"
outputIslandX AU  = "@"


-- | Output an upper-case Island character.
outputIslandX' :: (IsString a) => KwakLetter -> a
outputIslandX' M   = "M"
outputIslandX' MY  = "M{"
outputIslandX' N   = "N"
outputIslandX' NY  = "N{"
outputIslandX' P   = "P"
outputIslandX' T   = "T"
outputIslandX' B   = "B"
outputIslandX' D   = "D"
outputIslandX' PY  = "P{"
outputIslandX' TY  = "T{"
outputIslandX' TS  = "C"
outputIslandX' TL  = "["
outputIslandX' DZ  = "D+"
outputIslandX' DL  = "]"
outputIslandX' TSY = "C{"
outputIslandX' TLY = "[{"
outputIslandX' S   = "S"
outputIslandX' LH  = "<"
outputIslandX' L   = "L"
outputIslandX' LY  = "L`" -- "L{"
outputIslandX' J   = "Y"
outputIslandX' JY  = "Y{"
outputIslandX' K   = "K"
outputIslandX' KW  = "K#"
outputIslandX' G   = "G"
outputIslandX' GW  = "G#"
outputIslandX' KY  = "K{"
outputIslandX' KWY = "K{#"
outputIslandX' Q   = "Q"
outputIslandX' QW  = "Q#"
outputIslandX' GU  = "G}"
outputIslandX' GUW = "G}#"
outputIslandX' QY  = "Q{"
outputIslandX' QWY = "Q{#"
outputIslandX' X   = "X"
outputIslandX' XW  = "X#"
outputIslandX' XU  = "X}"
outputIslandX' XUW = "X}#"
outputIslandX' W   = "W"
outputIslandX' WY  = "W{"
outputIslandX' Y   = "%"
outputIslandX' H   = "H"
outputIslandX' A   = "A"
outputIslandX' E   = "E"
outputIslandX' I   = "I"
outputIslandX' O   = "O"
outputIslandX' U   = "U"
outputIslandX' AU  = "@"

-----------------------
-- Plain Strict Text --

outputIsland :: KwakLetter -> T.Text
outputIsland = outputIslandX

outputIsland' :: KwakLetter -> T.Text
outputIsland' = outputIslandX'

--------------
-- Builders --

outputIsland2 :: KwakLetter -> TL.Builder
outputIsland2 = outputIslandX

outputIsland2' :: KwakLetter -> TL.Builder
outputIsland2' = outputIslandX'

-------------------------------------------
-- Main output Functions

-- | Output for the "Island" font.
-- 
-- This version uses strict `T.Text` output.
decodeToIsland :: [CasedChar] -> T.Text
decodeToIsland = T.concat . (map $ mapChar $ mapCase outputIsland' outputIsland)

-- | Output for the "Island" font.
-- 
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToIslandLazy :: [CasedChar] -> TL.Text
decodeToIslandLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIsland2' outputIsland2))

-- | Output for the "Island" font.
-- 
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToIsland2 :: [CasedChar] -> T.Text
decodeToIsland2 = TL.toStrict . decodeToIslandLazy








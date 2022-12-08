{-|
Module      : Kwakwala.Output.Georgian
Description : Output for a Kwak'wala Orthography based on the Georgian Alphabet.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module has output functions for an
orthography for Kwak'wala based on the
Georgian alphabet. Since Georgian has
many ejective sounds with independent
letters, it is fairly easy to adapt the
alphabet to Kwak'wala.
-}


module Kwakwala.Output.Georgian
    -- * Exclusively Using Strict Text
    ( decodeToGeorgian
    , decodeToGeorgianAlt
    -- * Strict Text with Builders
    , decodeToGeorgian2
    , decodeToGeorgianAlt2
    -- * Lazy Text Output
    , decodeToGeorgianLazy
    , decodeToGeorgianLazyAlt
    ) where

import Data.Text              qualified as T
import Data.Text.IO           qualified as T
import Data.Text.Encoding     qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Control.Monad
-- import Control.Applicative

-- import Data.Functor
-- import Data.List
import Data.Char
import Data.String

import Kwakwala.Sounds

-- import Data.Either

import System.IO

-------------------------------------------
-- Using Standard Strict Text

-- Using the Georgian alphabet to write Kwak'wala
-- Basic changes: (-> : is represented as)
-- w  -> v
-- lh -> sh
-- gu -> gamma
-- x  -> r

-- | Georgian output using the Mkhedruli script.
-- This is the standard script used for most
-- Georgian text today.
-- Using `IsString` so that you only have
-- to write one function for multiple output
-- types.
outputGeorgianX :: (IsString a) => KwakLetter -> a
outputGeorgianX M   = "მ"
outputGeorgianX MY  = "ჸმ"
outputGeorgianX N   = "ნ"
outputGeorgianX NY  = "ჸნ"
outputGeorgianX P   = "ფ"
outputGeorgianX T   = "თ"
outputGeorgianX B   = "ბ"
outputGeorgianX D   = "დ"
outputGeorgianX PY  = "პ"
outputGeorgianX TY  = "ტ"
outputGeorgianX TS  = "ც"
outputGeorgianX TL  = "ჩ"
outputGeorgianX DZ  = "ძ"
outputGeorgianX DL  = "ჯ"
outputGeorgianX TSY = "წ"
outputGeorgianX TLY = "ჭ"
outputGeorgianX S   = "ს"
outputGeorgianX LH  = "შ"
outputGeorgianX L   = "ლ"
outputGeorgianX LY  = "ჸლ"
outputGeorgianX J   = "ჲ"
outputGeorgianX JY  = "ჸჲ"
outputGeorgianX K   = "ქ"
outputGeorgianX KW  = "ქვ"
outputGeorgianX G   = "გ"
outputGeorgianX GW  = "გვ"
outputGeorgianX KY  = "კ"
outputGeorgianX KWY = "კვ"
outputGeorgianX Q   = "ჴ"
outputGeorgianX QW  = "ჴვ"
outputGeorgianX GU  = "ღ"
outputGeorgianX GUW = "ღვ"
outputGeorgianX QY  = "ყ"
outputGeorgianX QWY = "ყვ"
outputGeorgianX X   = "რ"
outputGeorgianX XW  = "რვ"
outputGeorgianX XU  = "ხ"
outputGeorgianX XUW = "ხვ"
outputGeorgianX W   = "ვ"
outputGeorgianX WY  = "ჸვ"
outputGeorgianX Y   = "ჸ"
outputGeorgianX H   = "ჰ"
outputGeorgianX A   = "ა"
outputGeorgianX E   = "ე"
outputGeorgianX I   = "ი"
outputGeorgianX O   = "ო"
outputGeorgianX U   = "უ"
outputGeorgianX AU  = "ჷ"
-- outputGeorgian Spc  = " "
-- asdfzxcv

-- | Georgian output using (I think) the Asomtavruli script.
-- Only used if you want "upper-case" Georgian
-- letters.
-- Using `IsString` so that you only have
-- to write one function for multiple output
-- types.
outputGeorgianX' :: (IsString a) => KwakLetter -> a
outputGeorgianX' M   = "Ⴋ"
outputGeorgianX' MY  = "ჸႫ"
outputGeorgianX' N   = "Ⴌ"
outputGeorgianX' NY  = "ჸႬ"
outputGeorgianX' P   = "Ⴔ"
outputGeorgianX' T   = "Ⴇ"
outputGeorgianX' B   = "Ⴁ"
outputGeorgianX' D   = "Ⴃ"
outputGeorgianX' PY  = "Ⴎ"
outputGeorgianX' TY  = "Ⴒ"
outputGeorgianX' TS  = "Ⴚ"
outputGeorgianX' TL  = "Ⴙ"
outputGeorgianX' DZ  = "Ⴛ"
outputGeorgianX' DL  = "Ⴟ" -- \x323 = combining dot below -- Ⱡ
outputGeorgianX' TSY = "Ⴜ"
outputGeorgianX' TLY = "Ⴝ"
outputGeorgianX' S   = "Ⴑ"
outputGeorgianX' LH  = "Ⴘ"
outputGeorgianX' L   = "Ⴊ"
outputGeorgianX' LY  = "ჸႪ"
outputGeorgianX' J   = "Ⴢ"
outputGeorgianX' JY  = "ჸჂ"
outputGeorgianX' K   = "Ⴕ"
outputGeorgianX' KW  = "Ⴕვ"
outputGeorgianX' G   = "Ⴂ"
outputGeorgianX' GW  = "Ⴂვ"
outputGeorgianX' KY  = "Ⴉ"
outputGeorgianX' KWY = "Ⴉვ"
outputGeorgianX' Q   = "Ⴤ"
outputGeorgianX' QW  = "Ⴤვ"
outputGeorgianX' GU  = "Ⴖ" -- "g̣"
outputGeorgianX' GUW = "Ⴖვ" -- "g̣ᵘ"
outputGeorgianX' QY  = "Ⴗ"
outputGeorgianX' QWY = "Ⴗვ"
outputGeorgianX' X   = "Ⴐ"
outputGeorgianX' XW  = "Ⴐვ"
outputGeorgianX' XU  = "Ⴞ"
outputGeorgianX' XUW = "Ⴞვ"
outputGeorgianX' W   = "Ⴅ"
outputGeorgianX' WY  = "ჸႥ"
outputGeorgianX' Y   = "ჸ"
outputGeorgianX' H   = "Ⴠ"
outputGeorgianX' A   = "Ⴀ" -- a
outputGeorgianX' E   = "Ⴄ" -- a umlaut
outputGeorgianX' I   = "Ⴈ" -- i
outputGeorgianX' O   = "Ⴍ" -- a hatchet
outputGeorgianX' U   = "Ⴓ" -- u
outputGeorgianX' AU  = "ჷ"
-- asdfzxcv

----------------------
-- Possibilities for smallcaps L in uppercase

-- Ļ L-cedilla / L-comma
-- Ŀ L-middot
-- Ɫ L-tilde (also ɫ)
-- Ⱡ L-double stroke

outputGeorgian :: KwakLetter -> T.Text
outputGeorgian = outputGeorgianX

outputGeorgian' :: KwakLetter -> T.Text
outputGeorgian' = outputGeorgianX'


-- | Georgian output using both Mkhedruli and Asomtavruli scripts.
-- Use this if you want title-case text.
-- 
-- This version uses strict `Text` output.
decodeToGeorgian :: [CasedChar] -> T.Text
decodeToGeorgian = T.concat . (map $ mapChar $ mapCase outputGeorgian' outputGeorgian)

-- | Georgian Output exclusively in the Mkhedruli script.
-- 
-- This version uses strict `Text` output.
decodeToGeorgianAlt :: [CasedChar] -> T.Text
decodeToGeorgianAlt = T.concat . (map $ mapChar $ mapCase outputGeorgian outputGeorgian)

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputGeorgian2 :: KwakLetter -> TL.Builder
outputGeorgian2 = outputGeorgianX

outputGeorgian2' :: KwakLetter -> TL.Builder
outputGeorgian2' = outputGeorgianX'

-- | Georgian output using both Mkhedruli and Asomtavruli scripts.
-- Use this if you want title-case text.
-- 
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgian2 :: [CasedChar] -> T.Text
decodeToGeorgian2 = TL.toStrict . decodeToGeorgianLazy

-- | Georgian Output exclusively in the Mkhedruli script.
-- 
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgianAlt2 :: [CasedChar] -> T.Text
decodeToGeorgianAlt2 = TL.toStrict . decodeToGeorgianLazyAlt

-- | Georgian output using both Mkhedruli and Asomtavruli scripts.
-- Use this if you want title-case text.
-- 
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGeorgianLazy :: [CasedChar] -> TL.Text
decodeToGeorgianLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGeorgian2' outputGeorgian2))

-- | Georgian Output exclusively in the Mkhedruli script.
-- 
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGeorgianLazyAlt :: [CasedChar] -> TL.Text
decodeToGeorgianLazyAlt = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGeorgian2 outputGeorgian2))


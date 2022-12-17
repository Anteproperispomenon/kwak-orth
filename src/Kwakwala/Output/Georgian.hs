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
    , decodeToGeorgianTitle
    -- * Strict Text with Builders
    , decodeToGeorgian2
    , decodeToGeorgianAlt2
    , decodeToGeorgianTitle2
    -- * Lazy Text Output
    , decodeToGeorgianLazy
    , decodeToGeorgianLazyAlt
    , decodeToGeorgianLazyTitle
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

-- | Georgian output using the Asomtavruli script.
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
outputGeorgianX' DL  = "Ⴟ"
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
outputGeorgianX' GU  = "Ⴖ"
outputGeorgianX' GUW = "Ⴖვ"
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
outputGeorgianX' A   = "Ⴀ"
outputGeorgianX' E   = "Ⴄ"
outputGeorgianX' I   = "Ⴈ"
outputGeorgianX' O   = "Ⴍ"
outputGeorgianX' U   = "Ⴓ"
outputGeorgianX' AU  = "Ⴧ"

-- | Georgian output using the Mtavruli script.
-- This is an alternate version of Mkhedruli
-- that is used for all-caps text in Georgian.
--
-- Using `IsString` so that you only have
-- to write one function for multiple output
-- types.
outputGeorgianX'' :: (IsString a) => KwakLetter -> a
outputGeorgianX'' M   = "Მ"
outputGeorgianX'' MY  = "Ჸმ"
outputGeorgianX'' N   = "Ნ"
outputGeorgianX'' NY  = "Ჸნ"
outputGeorgianX'' P   = "Ფ"
outputGeorgianX'' T   = "Თ"
outputGeorgianX'' B   = "Ბ"
outputGeorgianX'' D   = "Დ"
outputGeorgianX'' PY  = "Პ"
outputGeorgianX'' TY  = "Ტ"
outputGeorgianX'' TS  = "Ც"
outputGeorgianX'' TL  = "Ჩ"
outputGeorgianX'' DZ  = "Ძ"
outputGeorgianX'' DL  = "Ჯ"
outputGeorgianX'' TSY = "Წ"
outputGeorgianX'' TLY = "Ჭ"
outputGeorgianX'' S   = "Ს"
outputGeorgianX'' LH  = "Შ"
outputGeorgianX'' L   = "Ლ"
outputGeorgianX'' LY  = "Ჸლ"
outputGeorgianX'' J   = "Ჲ"
outputGeorgianX'' JY  = "Ჸჲ"
outputGeorgianX'' K   = "Ქ"
outputGeorgianX'' KW  = "Ქვ"
outputGeorgianX'' G   = "Გ"
outputGeorgianX'' GW  = "Გვ"
outputGeorgianX'' KY  = "Კ"
outputGeorgianX'' KWY = "Კვ"
outputGeorgianX'' Q   = "Ჴ"
outputGeorgianX'' QW  = "Ჴვ"
outputGeorgianX'' GU  = "Ღ"
outputGeorgianX'' GUW = "Ღვ"
outputGeorgianX'' QY  = "Ყ"
outputGeorgianX'' QWY = "Ყვ"
outputGeorgianX'' X   = "Რ"
outputGeorgianX'' XW  = "Რვ"
outputGeorgianX'' XU  = "Ხ"
outputGeorgianX'' XUW = "Ხვ"
outputGeorgianX'' W   = "Ვ"
outputGeorgianX'' WY  = "Ჸვ"
outputGeorgianX'' Y   = "Ჸ"
outputGeorgianX'' H   = "Ჰ"
outputGeorgianX'' A   = "Ა"
outputGeorgianX'' E   = "Ე"
outputGeorgianX'' I   = "Ი"
outputGeorgianX'' O   = "Ო"
outputGeorgianX'' U   = "Უ"
outputGeorgianX'' AU  = "Ჷ"

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

outputGeorgian'' :: KwakLetter -> T.Text
outputGeorgian'' = outputGeorgianX''

-- | Georgian output using both Mkhedruli and Asomtavruli scripts.
-- Use this if you want title-case text.
-- 
-- This version uses strict `T.Text` output.
decodeToGeorgian :: [CasedChar] -> T.Text
decodeToGeorgian = T.concat . (map $ mapChar $ mapCase outputGeorgian' outputGeorgian)

-- | Georgian Output exclusively in the Mkhedruli script.
-- 
-- This version uses strict `T.Text` output.
decodeToGeorgianAlt :: [CasedChar] -> T.Text
decodeToGeorgianAlt = T.concat . (map $ mapChar $ mapCase outputGeorgian outputGeorgian)

-- | Georgian Output using both Mkhedruli and Mtavruli scripts.
-- Use this for an alternate version of title-case text,
-- where upper-case letters are much closer in appearance
-- to their lower-case counterpart.
-- 
-- This version uses strict `T.Text` output.
decodeToGeorgianTitle :: [CasedChar] -> T.Text
decodeToGeorgianTitle = T.concat . (map $ mapChar $ mapCase outputGeorgian'' outputGeorgian)

--------------------------------------------
-- Using Builders

-- Builder-based lower-case/Mkhedruli letter output
outputGeorgian2 :: KwakLetter -> TL.Builder
outputGeorgian2 = outputGeorgianX

-- Builder-based Asomtavruli letter output
outputGeorgian2' :: KwakLetter -> TL.Builder
outputGeorgian2' = outputGeorgianX'

-- Builder-based Mtavruli letter output
outputGeorgian2'' :: KwakLetter -> TL.Builder
outputGeorgian2'' = outputGeorgianX''

-- | Georgian output using both Mkhedruli and Asomtavruli scripts.
-- Use this if you want title-case text.
-- 
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgian2 :: [CasedChar] -> T.Text
decodeToGeorgian2 = TL.toStrict . decodeToGeorgianLazy

-- | Georgian Output exclusively in the Mkhedruli script.
-- 
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgianAlt2 :: [CasedChar] -> T.Text
decodeToGeorgianAlt2 = TL.toStrict . decodeToGeorgianLazyAlt

-- | Georgian Output using both Mkhedruli and Mtavruli scripts.
-- Use this for an alternate version of title-case text,
-- where upper-case letters are much closer in appearance
-- to their lower-case counterpart.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgianTitle2 :: [CasedChar] -> T.Text
decodeToGeorgianTitle2 = TL.toStrict . decodeToGeorgianLazyTitle


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

-- | Georgian Output using both Mkhedruli and Mtavruli scripts.
-- Use this for an alternate version of title-case text,
-- where upper-case letters are much closer in appearance
-- to their lower-case counterpart.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGeorgianLazyTitle :: [CasedChar] -> TL.Text
decodeToGeorgianLazyTitle = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGeorgian2'' outputGeorgian2))


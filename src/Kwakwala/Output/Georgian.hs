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
    , decodeToGeorgianC
    , decodeToGeorgianAlt
    , decodeToGeorgianTitle
    -- * Strict Text with Builders
    , decodeToGeorgian2
    , decodeToGeorgianC2
    , decodeToGeorgianAlt2
    , decodeToGeorgianTitle2
    -- * Lazy Text Output
    , decodeToGeorgianLazy
    , decodeToGeorgianLazyC
    , decodeToGeorgianLazyAlt
    , decodeToGeorgianLazyTitle
    -- * Configuration Type
    , GeorgianOutputConfig(..)
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

-------------------------------------------
-- With custom options

-- | Options for outputting different. The use of the
-- hard sign to indicate palatalisation (which isn't
-- normally indicated) is for helping to train
-- speech-to-text algorithms, which might be confused
-- by representing (kw) as (kj + w).
data GeorgianOutputConfig = GeorgianOutputConfig
  { _gocUseLabSign :: Bool -- Use Abkhaz labialisation sign. (ჿᲿ)
  , _gocUsePalSign :: Bool -- Use Abkhaz 'hard' sign. (ჾᲾ)
  } deriving (Show, Eq)

-- | Georgian output using the Mkhedruli script.
-- This is the standard script used for most
-- Georgian text today.
-- Using `IsString` so that you only have
-- to write one function for multiple output
-- types.
outputGeorgianC :: (IsString a) => GeorgianOutputConfig -> KwakLetter -> a
outputGeorgianC _ M   = "მ"
outputGeorgianC _ MY  = "ჸმ"
outputGeorgianC _ N   = "ნ"
outputGeorgianC _ NY  = "ჸნ"
outputGeorgianC _ P   = "ფ"
outputGeorgianC _ T   = "თ"
outputGeorgianC _ B   = "ბ"
outputGeorgianC _ D   = "დ"
outputGeorgianC _ PY  = "პ"
outputGeorgianC _ TY  = "ტ"
outputGeorgianC _ TS  = "ც"
outputGeorgianC _ TL  = "ჩ"
outputGeorgianC _ DZ  = "ძ"
outputGeorgianC _ DL  = "ჯ"
outputGeorgianC _ TSY = "წ"
outputGeorgianC _ TLY = "ჭ"
outputGeorgianC _ S   = "ს"
outputGeorgianC _ LH  = "შ"
outputGeorgianC _ L   = "ლ"
outputGeorgianC _ LY  = "ჸლ"
outputGeorgianC _ J   = "ჲ"
outputGeorgianC _ JY  = "ჸჲ"
outputGeorgianC goc K 
  | (_gocUsePalSign goc) = "ქჾ"
  | otherwise            = "ქ"
outputGeorgianC goc KW 
  | (_gocUseLabSign goc) = "ქჿ"
  | otherwise = "ქვ"
outputGeorgianC goc G
  | (_gocUsePalSign goc) = "გჾ"
  | otherwise            = "გ"
outputGeorgianC goc GW
  | (_gocUseLabSign goc) = "გჿ"
  | otherwise = "გვ"
outputGeorgianC goc KY
  | (_gocUsePalSign goc) = "კჾ"
  | otherwise            = "კ"
outputGeorgianC goc KWY
  | (_gocUseLabSign goc) = "კჿ"
  | otherwise = "კვ"
outputGeorgianC _ Q   = "ჴ"
outputGeorgianC goc QW
  | (_gocUseLabSign goc) = "ჴჿ"
  | otherwise = "ჴვ"
outputGeorgianC _ GU  = "ღ"
outputGeorgianC goc GUW
  | (_gocUseLabSign goc) = "ღჿ"
  | otherwise = "ღვ"
outputGeorgianC _ QY  = "ყ"
outputGeorgianC goc QWY
  | (_gocUseLabSign goc) = "ყჿ"
  | otherwise = "ყვ"
outputGeorgianC goc X
  | (_gocUsePalSign goc) = "რჾ"
  | otherwise            = "რ"
outputGeorgianC goc XW
  | (_gocUseLabSign goc) = "რჿ"
  | otherwise = "რვ"
outputGeorgianC _ XU  = "ხ"
outputGeorgianC goc XUW
  | (_gocUseLabSign goc) = "ხჿ"
  | otherwise = "ხვ"
outputGeorgianC _ W   = "ვ"
outputGeorgianC _ WY  = "ჸვ"
outputGeorgianC _ Y   = "ჸ"
outputGeorgianC _ H   = "ჰ"
outputGeorgianC _ A   = "ა"
outputGeorgianC _ E   = "ე"
outputGeorgianC _ I   = "ი"
outputGeorgianC _ O   = "ო"
outputGeorgianC _ U   = "უ"
outputGeorgianC _ AU  = "ჷ"

outputGeorgianC' :: (IsString a) => GeorgianOutputConfig -> KwakLetter -> a
outputGeorgianC' _ M   = "Მ"
outputGeorgianC' _ MY  = "Ჸმ"
outputGeorgianC' _ N   = "Ნ"
outputGeorgianC' _ NY  = "Ჸნ"
outputGeorgianC' _ P   = "Ფ"
outputGeorgianC' _ T   = "Თ"
outputGeorgianC' _ B   = "Ბ"
outputGeorgianC' _ D   = "Დ"
outputGeorgianC' _ PY  = "Პ"
outputGeorgianC' _ TY  = "Ტ"
outputGeorgianC' _ TS  = "Ც"
outputGeorgianC' _ TL  = "Ჩ"
outputGeorgianC' _ DZ  = "Ძ"
outputGeorgianC' _ DL  = "Ჯ"
outputGeorgianC' _ TSY = "Წ"
outputGeorgianC' _ TLY = "Ჭ"
outputGeorgianC' _ S   = "Ს"
outputGeorgianC' _ LH  = "Შ"
outputGeorgianC' _ L   = "Ლ"
outputGeorgianC' _ LY  = "Ჸლ"
outputGeorgianC' _ J   = "Ჲ"
outputGeorgianC' _ JY  = "Ჸჲ"
outputGeorgianC' goc K
  | (_gocUsePalSign goc) = "Ქჾ"
  | otherwise            = "Ქ"
outputGeorgianC' goc KW
  | (_gocUseLabSign goc) = "Ქჿ"
  | otherwise = "Ქვ"
outputGeorgianC' goc G
  | (_gocUsePalSign goc) = "Გჾ"
  | otherwise            = "Გ"
outputGeorgianC' goc GW
  | (_gocUseLabSign goc) = "Გჿ"
  | otherwise = "Გვ"
outputGeorgianC' goc KY
  | (_gocUsePalSign goc) = "Კჾ"
  | otherwise            = "Კ"
outputGeorgianC' goc KWY
  | (_gocUseLabSign goc) = "Კჿ"
  | otherwise = "Კვ"
outputGeorgianC' _ Q   = "Ჴ"
outputGeorgianC' goc QW
  | (_gocUseLabSign goc) = "Ჴჿ"
  | otherwise = "Ჴვ"
outputGeorgianC' _ GU  = "Ღ"
outputGeorgianC' goc GUW
  | (_gocUseLabSign goc) = "Ღჿ"
  | otherwise = "Ღვ"
outputGeorgianC' _ QY  = "Ყ"
outputGeorgianC' goc QWY
  | (_gocUseLabSign goc) = "Ყჿ"
  | otherwise = "Ყვ"
outputGeorgianC' goc X
  | (_gocUsePalSign goc) = "Რჾ"
  | otherwise            = "Რ"
outputGeorgianC' goc XW
  | (_gocUseLabSign goc) = "Რჿ"
  | otherwise = "Რვ"
outputGeorgianC' _ XU  = "Ხ"
outputGeorgianC' goc XUW
  | (_gocUseLabSign goc) = "Ხჿ"
  | otherwise = "Ხვ"
outputGeorgianC' _ W   = "Ვ"
outputGeorgianC' _ WY  = "Ჸვ"
outputGeorgianC' _ Y   = "Ჸ"
outputGeorgianC' _ H   = "Ჰ"
outputGeorgianC' _ A   = "Ა"
outputGeorgianC' _ E   = "Ე"
outputGeorgianC' _ I   = "Ი"
outputGeorgianC' _ O   = "Ო"
outputGeorgianC' _ U   = "Უ"
outputGeorgianC' _ AU  = "Ჷ"


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

outputGeorgianCT :: GeorgianOutputConfig -> KwakLetter -> T.Text
outputGeorgianCT = outputGeorgianC 

outputGeorgianCT' :: GeorgianOutputConfig -> KwakLetter -> T.Text
outputGeorgianCT' = outputGeorgianC'

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

-- | Georgian Output using both Mkhedruli and Mtavruli scripts,
-- with customisable options. Use this when you want
-- more control over the form of the output of your text.
-- 
-- This version uses strict `T.Text` output.
decodeToGeorgianC :: GeorgianOutputConfig -> [CasedChar] -> T.Text
decodeToGeorgianC goc = T.concat . (map $ mapChar $ mapCase (outputGeorgianCT' goc) (outputGeorgianCT goc))

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

-- Builder-based configurable lower-case/Mkhedruli letter output
outputGeorgian2C :: GeorgianOutputConfig -> KwakLetter -> TL.Builder
outputGeorgian2C = outputGeorgianC


-- Builder-based configurable Mtavruli letter output
outputGeorgian2C' :: GeorgianOutputConfig -> KwakLetter -> TL.Builder
outputGeorgian2C' = outputGeorgianC'

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

-- | Georgian Output using both Mkhedruli and Mtavruli scripts,
-- with customisable options. Use this when you want
-- more control over the form of the output of your text.
-- 
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGeorgianC2 :: GeorgianOutputConfig -> [CasedChar] -> T.Text
decodeToGeorgianC2 goc = TL.toStrict . (decodeToGeorgianLazyC goc)

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

-- | Georgian Output using both Mkhedruli and Mtavruli scripts,
-- with customisable options. Use this when you want
-- more control over the form of the output of your text.
-- 
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGeorgianLazyC :: GeorgianOutputConfig -> [CasedChar] -> TL.Text
decodeToGeorgianLazyC goc = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase (outputGeorgian2C' goc) (outputGeorgian2C goc)))


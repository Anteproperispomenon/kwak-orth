{-|
Module      : Kwakwala.Output.UmistaOutputNew
Description : U'mista output for Kwak'wala.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains output functions for
the U'mista orthography for Kwak'wala. 
U'mista is the most commonly used orthography
for northern dialects of Kwak'wala. This is
one of the most commonly used output modules
in this library.

For more information about the U'mista orthography,
see <http://www.languagegeek.com/wakashan/kwakwala.html>.

-}

module Kwakwala.Output.UmistaOutputNew
    -- * Exclusively Using Strict Text
    ( decodeToUmista
    , decodeToUmistaAlt
    -- * Strict Text with Builders
    , decodeToUmista2
    , decodeToUmistaAlt2
    -- * Lazy Text Output
    , decodeToUmistaLazy
    , decodeToUmistaAltLazy
    ) where

import Data.Text              qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Data.List (groupBy)
import Kwakwala.Sounds

-------------------------------------------
-- Using Standard Strict Text

-- Apostrophes
-- Glottal Stop: U+0027
-- Ejective, Single Consonants : U+0313 ("\x313")
-- Ejective, Double Consonants : U+0315 ("\x315") (in between the two letters)

-- Note: Be careful when using writing characters
-- as their codepoint, both in decimal and hexadecimal.
-- If the codepoint is followed by a [hexa]decimal character[s],
-- that/those character[s] will be interpreted as being
-- part of the code point.
-- e.g. "k\x313ata" will be interpreted as
-- ['k','\x313a','t','a'] instead of
-- ['k','\x313','a','t','a'].
-- To avoid this, put an '\&' character after
-- the codepoint; this shows that the following
-- character[s] are NOT part of the codepoint,
-- and should just be interpreted as themselves.

outputUmista :: KwakLetter -> T.Text
outputUmista M   = "m"
outputUmista MY  = "'m"
outputUmista N   = "n"
outputUmista NY  = "'n"
outputUmista P   = "p"
outputUmista T   = "t"
outputUmista B   = "b"
outputUmista D   = "d"
outputUmista PY  = "p\x313"
outputUmista TY  = "t\x313"
outputUmista TS  = "ts"
outputUmista TL  = "tł"
outputUmista DZ  = "dz"
outputUmista DL  = "dł"
outputUmista TSY = "t\x315s"
outputUmista TLY = "t\x315ł"
outputUmista S   = "s"
outputUmista LH  = "ł"
outputUmista L   = "l"
outputUmista LY  = "'l" -- "l\x313"
outputUmista J   = "y"
outputUmista JY  = "'y"
outputUmista K   = "k"
outputUmista KW  = "kw"
outputUmista G   = "g"
outputUmista GW  = "gw"
outputUmista KY  = "k\x313"
outputUmista KWY = "k\x315w"
outputUmista Q   = "ḵ"
outputUmista QW  = "ḵw"
outputUmista GU  = "g̱"
outputUmista GUW = "g̱w"
outputUmista QY  = "ḵ\x313"
outputUmista QWY = "ḵ\x315w"
outputUmista X   = "x"
outputUmista XW  = "xw"
outputUmista XU  = "x̱"
outputUmista XUW = "x̱w"
outputUmista W   = "w"
outputUmista WY  = "'w"
outputUmista Y   = "'"
outputUmista H   = "h"
outputUmista A   = "a"
outputUmista E   = "e"
outputUmista I   = "i"
outputUmista O   = "o"
outputUmista U   = "u"
outputUmista AU  = "a̱"

-- Title Case
outputUmista' :: KwakLetter -> T.Text
outputUmista' M   = "M"
outputUmista' MY  = "'M"
outputUmista' N   = "N"
outputUmista' NY  = "'N"
outputUmista' P   = "P"
outputUmista' T   = "T"
outputUmista' B   = "B"
outputUmista' D   = "D"
outputUmista' PY  = "P\x313"
outputUmista' TY  = "T\x313"
outputUmista' TS  = "Ts"
outputUmista' TL  = "Tł"
outputUmista' DZ  = "Dz"
outputUmista' DL  = "Dł"
outputUmista' TSY = "T\x315s"
outputUmista' TLY = "T\x315ł"
outputUmista' S   = "S"
outputUmista' LH  = "Ł"
outputUmista' L   = "L"
outputUmista' LY  = "'L"
outputUmista' J   = "Y"
outputUmista' JY  = "'Y"
outputUmista' K   = "K"
outputUmista' KW  = "Kw"
outputUmista' G   = "G"
outputUmista' GW  = "Gw"
outputUmista' KY  = "K\x313"
outputUmista' KWY = "K\x315w"
outputUmista' Q   = "Ḵ"
outputUmista' QW  = "Ḵw"
outputUmista' GU  = "G\x331"
outputUmista' GUW = "G\x331w"
outputUmista' QY  = "Ḵ\x313"
outputUmista' QWY = "Ḵ\x315w"
outputUmista' X   = "X"
outputUmista' XW  = "Xw"
outputUmista' XU  = "X\x331"
outputUmista' XUW = "X\x331w"
outputUmista' W   = "W"
outputUmista' WY  = "'W"
outputUmista' Y   = "'"
outputUmista' H   = "H"
outputUmista' A   = "A"
outputUmista' E   = "E"
outputUmista' I   = "I"
outputUmista' O   = "O"
outputUmista' U   = "U"
outputUmista' AU  = "A\x331"

-- Upper Case
outputUmista'' :: KwakLetter -> T.Text
outputUmista'' M   = "M"
outputUmista'' MY  = "'M"
outputUmista'' N   = "N"
outputUmista'' NY  = "'N"
outputUmista'' P   = "P"
outputUmista'' T   = "T"
outputUmista'' B   = "B"
outputUmista'' D   = "D"
outputUmista'' PY  = "P\x313"
outputUmista'' TY  = "T\x313"
outputUmista'' TS  = "TS"
outputUmista'' TL  = "TŁ"
outputUmista'' DZ  = "DZ"
outputUmista'' DL  = "DŁ"
outputUmista'' TSY = "T\x315S"
outputUmista'' TLY = "T\x315Ł"
outputUmista'' S   = "S"
outputUmista'' LH  = "Ł"
outputUmista'' L   = "L"
outputUmista'' LY  = "'L"
outputUmista'' J   = "Y"
outputUmista'' JY  = "'Y"
outputUmista'' K   = "K"
outputUmista'' KW  = "KW"
outputUmista'' G   = "G"
outputUmista'' GW  = "GW"
outputUmista'' KY  = "K\x313"
outputUmista'' KWY = "K\x315W"
outputUmista'' Q   = "Ḵ"
outputUmista'' QW  = "ḴW"
outputUmista'' GU  = "G\x331"
outputUmista'' GUW = "G\x331W"
outputUmista'' QY  = "Ḵ\x313"
outputUmista'' QWY = "Ḵ\x315W"
outputUmista'' X   = "X"
outputUmista'' XW  = "XW"
outputUmista'' XU  = "X\x331"
outputUmista'' XUW = "X\x331W"
outputUmista'' W   = "W"
outputUmista'' WY  = "'W"
outputUmista'' Y   = "'"
outputUmista'' H   = "H"
outputUmista'' A   = "A"
outputUmista'' E   = "E"
outputUmista'' I   = "I"
outputUmista'' O   = "O"
outputUmista'' U   = "U"
outputUmista'' AU  = "A\x331"

-- Alternate version where the apostrophe
-- comes after the consonant for 
-- glottalised sonorants.
outputUmistaAlt :: KwakLetter -> T.Text 
outputUmistaAlt MY = "m\x313"
outputUmistaAlt NY = "n\x313"
outputUmistaAlt LY = "l\x313"
outputUmistaAlt JY = "y\x313"
outputUmistaAlt WY = "w\x313"
outputUmistaAlt x  = outputUmista x

-- Alternate version where the apostrophe
-- comes after the consonant for 
-- glottalised sonorants.
outputUmistaAlt' :: KwakLetter -> T.Text 
outputUmistaAlt' MY = "M\x313"
outputUmistaAlt' NY = "N\x313"
outputUmistaAlt' LY = "L\x313"
outputUmistaAlt' JY = "Y\x313"
outputUmistaAlt' WY = "W\x313"
outputUmistaAlt' x  = outputUmista' x

-- | Standard U'mista text output.
--
-- This version uses strict `T.Text` output.
decodeToUmista :: [CasedChar] -> T.Text
decodeToUmista = decodeToUmistaY

-- Strict Text-based output
decodeToUmistaMain :: [CasedChar] -> T.Text
decodeToUmistaMain = T.concat . (map $ mapChar $ mapCase outputUmista' outputUmista)

-- Using the same char for ejectives and glottaliseds
decodeToUmistaAltZ :: [CasedChar] -> T.Text
decodeToUmistaAltZ = T.concat . (map $ mapChar $ mapCase outputUmistaAlt' outputUmistaAlt)

-- | Alternate U'mista text output, where
-- glottalized sonorants use the same type
-- of apostrophe as ejective consonants,
-- and places it __after__ the consonant,
-- rather than before.
--
-- This version uses strict `T.Text` output.
decodeToUmistaAlt :: [CasedChar] -> T.Text
decodeToUmistaAlt xs = decodeToUmistaAltZ $ decodeToUmistaYnew [] $ groupBy isSameCaseType xs

decodeToUmistaYold :: [CasedChar] -> T.Text
decodeToUmistaYold xs = decodeToUmistaMain $ concat $ decodeToUmistaY' $ groupBy isSameCaseType xs

-- Taking the initial glottal stop into account
decodeToUmistaY :: [CasedChar] -> T.Text
decodeToUmistaY xs = decodeToUmistaMain $ decodeToUmistaYnew [] $ groupBy isSameCaseType xs

-- right-fold
decodeToUmistaY' :: [[CasedChar]] -> [[CasedChar]]
decodeToUmistaY' [] = []
decodeToUmistaY' ((x@(Kwak z1) : y@(Kwak z2) : xs) : xss)
    | (isCharLetter Y x) && (isKwkVow' z2) = ((y:xs)   : (decodeToUmistaY' xss))
    | otherwise                            = ((x:y:xs) : (decodeToUmistaY' xss))
decodeToUmistaY' (xs : xss) = (xs : (decodeToUmistaY' xss))

-- left-fold
decodeToUmistaYnew :: [[CasedChar]] -> [[CasedChar]] -> [CasedChar]
decodeToUmistaYnew acc [] = concat $ reverse acc
decodeToUmistaYnew acc ((x@(Kwak z1) : y@(Kwak z2) : xs) : xss)
    | (isCharLetter Y x) && (isKwkVow' z2) = ((decodeToUmistaYnew ((  y:xs):acc) xss))
    | otherwise                            = ((decodeToUmistaYnew ((x:y:xs):acc) xss))
decodeToUmistaYnew acc (xs : xss) = (decodeToUmistaYnew (xs:acc) xss)

-- Set up glottal stops.
setupGlottal :: [CasedChar] -> [CasedChar]
setupGlottal xs = decodeToUmistaYnew [] $ groupBy isSameCaseType xs

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputUmista2 :: KwakLetter -> TL.Builder
outputUmista2 M   = "m"
outputUmista2 MY  = "'m"
outputUmista2 N   = "n"
outputUmista2 NY  = "'n"
outputUmista2 P   = "p"
outputUmista2 T   = "t"
outputUmista2 B   = "b"
outputUmista2 D   = "d"
outputUmista2 PY  = "p\x313"
outputUmista2 TY  = "t\x313"
outputUmista2 TS  = "ts"
outputUmista2 TL  = "tł"
outputUmista2 DZ  = "dz"
outputUmista2 DL  = "dł"
outputUmista2 TSY = "t\x315s"
outputUmista2 TLY = "t\x315ł"
outputUmista2 S   = "s"
outputUmista2 LH  = "ł"
outputUmista2 L   = "l"
outputUmista2 LY  = "'l" -- "l\x313"
outputUmista2 J   = "y"
outputUmista2 JY  = "'y"
outputUmista2 K   = "k"
outputUmista2 KW  = "kw"
outputUmista2 G   = "g"
outputUmista2 GW  = "gw"
outputUmista2 KY  = "k\x313"
outputUmista2 KWY = "k\x315w"
outputUmista2 Q   = "ḵ"
outputUmista2 QW  = "ḵw"
outputUmista2 GU  = "g̱"
outputUmista2 GUW = "g̱w"
outputUmista2 QY  = "ḵ\x313"
outputUmista2 QWY = "ḵ\x315w"
outputUmista2 X   = "x"
outputUmista2 XW  = "xw"
outputUmista2 XU  = "x̱"
outputUmista2 XUW = "x̱w"
outputUmista2 W   = "w"
outputUmista2 WY  = "'w"
outputUmista2 Y   = "'"
outputUmista2 H   = "h"
outputUmista2 A   = "a"
outputUmista2 E   = "e"
outputUmista2 I   = "i"
outputUmista2 O   = "o"
outputUmista2 U   = "u"
outputUmista2 AU  = "a̱"

-- Title Case
outputUmista2' :: KwakLetter -> TL.Builder
outputUmista2' M   = "M"
outputUmista2' MY  = "'M"
outputUmista2' N   = "N"
outputUmista2' NY  = "'N"
outputUmista2' P   = "P"
outputUmista2' T   = "T"
outputUmista2' B   = "B"
outputUmista2' D   = "D"
outputUmista2' PY  = "P\x313"
outputUmista2' TY  = "T\x313"
outputUmista2' TS  = "Ts"
outputUmista2' TL  = "Tł"
outputUmista2' DZ  = "Dz"
outputUmista2' DL  = "Dł"
outputUmista2' TSY = "T\x315s"
outputUmista2' TLY = "T\x315ł"
outputUmista2' S   = "S"
outputUmista2' LH  = "Ł"
outputUmista2' L   = "L"
outputUmista2' LY  = "'L"
outputUmista2' J   = "Y"
outputUmista2' JY  = "'Y"
outputUmista2' K   = "K"
outputUmista2' KW  = "Kw"
outputUmista2' G   = "G"
outputUmista2' GW  = "Gw"
outputUmista2' KY  = "K\x313"
outputUmista2' KWY = "K\x315w"
outputUmista2' Q   = "Ḵ"
outputUmista2' QW  = "Ḵw"
outputUmista2' GU  = "G\x331"
outputUmista2' GUW = "G\x331w"
outputUmista2' QY  = "Ḵ\x313"
outputUmista2' QWY = "Ḵ\x315w"
outputUmista2' X   = "X"
outputUmista2' XW  = "Xw"
outputUmista2' XU  = "X\x331"
outputUmista2' XUW = "X\x331w"
outputUmista2' W   = "W"
outputUmista2' WY  = "'W"
outputUmista2' Y   = "'"
outputUmista2' H   = "H"
outputUmista2' A   = "A"
outputUmista2' E   = "E"
outputUmista2' I   = "I"
outputUmista2' O   = "O"
outputUmista2' U   = "U"
outputUmista2' AU  = "A\x331"

-- Builder-Based Upper Case
outputUmista2'' :: KwakLetter -> TL.Builder
outputUmista2'' M   = "M"
outputUmista2'' MY  = "'M"
outputUmista2'' N   = "N"
outputUmista2'' NY  = "'N"
outputUmista2'' P   = "P"
outputUmista2'' T   = "T"
outputUmista2'' B   = "B"
outputUmista2'' D   = "D"
outputUmista2'' PY  = "P\x313"
outputUmista2'' TY  = "T\x313"
outputUmista2'' TS  = "TS"
outputUmista2'' TL  = "TŁ" -- ł
outputUmista2'' DZ  = "DZ"
outputUmista2'' DL  = "DŁ"
outputUmista2'' TSY = "T\x315S"
outputUmista2'' TLY = "T\x315Ł"
outputUmista2'' S   = "S"
outputUmista2'' LH  = "Ł"
outputUmista2'' L   = "L"
outputUmista2'' LY  = "'L"
outputUmista2'' J   = "Y"
outputUmista2'' JY  = "'Y"
outputUmista2'' K   = "K"
outputUmista2'' KW  = "KW"
outputUmista2'' G   = "G"
outputUmista2'' GW  = "GW"
outputUmista2'' KY  = "K\x313"
outputUmista2'' KWY = "K\x315W"
outputUmista2'' Q   = "Ḵ"
outputUmista2'' QW  = "ḴW"
outputUmista2'' GU  = "G\x331"
outputUmista2'' GUW = "G\x331W"
outputUmista2'' QY  = "Ḵ\x313"
outputUmista2'' QWY = "Ḵ\x315W"
outputUmista2'' X   = "X"
outputUmista2'' XW  = "XW"
outputUmista2'' XU  = "X\x331"
outputUmista2'' XUW = "X\x331W"
outputUmista2'' W   = "W"
outputUmista2'' WY  = "'W"
outputUmista2'' Y   = "'"
outputUmista2'' H   = "H"
outputUmista2'' A   = "A"
outputUmista2'' E   = "E"
outputUmista2'' I   = "I"
outputUmista2'' O   = "O"
outputUmista2'' U   = "U"
outputUmista2'' AU  = "A\x331"

outputUmistaAlt2 :: KwakLetter -> TL.Builder
outputUmistaAlt2 MY = "m\x313"
outputUmistaAlt2 NY = "n\x313"
outputUmistaAlt2 LY = "l\x313"
outputUmistaAlt2 JY = "y\x313"
outputUmistaAlt2 WY = "w\x313"
outputUmistaAlt2 x  = outputUmista2 x

outputUmistaAlt2' :: KwakLetter -> TL.Builder
outputUmistaAlt2' MY = "M\x313"
outputUmistaAlt2' NY = "N\x313"
outputUmistaAlt2' LY = "L\x313"
outputUmistaAlt2' JY = "Y\x313"
outputUmistaAlt2' WY = "W\x313"
outputUmistaAlt2' x  = outputUmista2' x

-- | Standard U'mista text output.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToUmista2 :: [CasedChar] -> T.Text
decodeToUmista2 = TL.toStrict . decodeToUmistaLazy

-- | Standard U'mista text output.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToUmistaLazy :: [CasedChar] -> TL.Text
decodeToUmistaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputUmista2' outputUmista2)) . setupGlottal

-- | Alternate U'mista text output, where
-- glottalized sonorants use the same type
-- of apostrophe as ejective consonants,
-- and places it __after__ the consonant,
-- rather than before.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToUmistaAlt2 :: [CasedChar] -> T.Text
decodeToUmistaAlt2 = TL.toStrict . decodeToUmistaAltLazy

-- | Alternate U'mista text output, where
-- glottalized sonorants use the same type
-- of apostrophe as ejective consonants,
-- and places it __after__ the consonant,
-- rather than before.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToUmistaAltLazy :: [CasedChar] -> TL.Text
decodeToUmistaAltLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputUmistaAlt2' outputUmistaAlt2)) . setupGlottal


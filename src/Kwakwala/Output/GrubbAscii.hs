{-|
Module      : Kwakwala.Output.GrubbAscii
Description : Output for an ASCII-compatible orthography.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module has output functions for an
orthography for Kwak'wala based on the
Grubb orthography, but modified to be
usable for ASCII. 

Note that since the main version of
Grubb-ASCII uses digraphs that end
in \"h\", you cannot have e.g. \"g\"
followed by \"h\", since that would be
interpreted as a uvular \"g\", rather
than velar \"g\" followed by a glottal
fricative.

As a result, there is an alternate version
of Grubb-ASCII where the sound \/h\/ is
represented by \"J\/j\" instead. Functions
that have a \"J\" near the end use this 
version instead.

Also, in standard Grubb-ASCII, glottal
stops followed by vowels at the beginning
of a word are omitted. If you want to
__keep__ the glottal stops at the beginnings
of words, use the functions that have an \"X\"
near the end.
-}

module Kwakwala.Output.GrubbAscii
    -- * Exclusively Using Strict Text
    ( decodeToGrubbAscii
    , decodeToGrubbAsciiX
    , decodeToGrubbAsciiJ
    , decodeToGrubbAsciiJX
    -- * Strict Text with Builders
    , decodeToGrubbAscii2
    , decodeToGrubbAsciiX2
    , decodeToGrubbAsciiJ2
    , decodeToGrubbAsciiJX2
    -- * Lazy Text Output
    , decodeToGrubbAsciiLazy
    , decodeToGrubbAsciiLazyX
    , decodeToGrubbAsciiLazyJ
    , decodeToGrubbAsciiLazyJX
    ) where

import Data.Text              qualified as T
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Data.List (groupBy)

import Kwakwala.Sounds

-------------------------------------------
-- Using Standard Strict Text

-- | Output a lower-case Grubb-ASCII character.
outputGrubbAscii :: KwakLetter -> T.Text
outputGrubbAscii M   = "m"
outputGrubbAscii MY  = "m'"
outputGrubbAscii N   = "n"
outputGrubbAscii NY  = "n'"
outputGrubbAscii P   = "p"
outputGrubbAscii T   = "t"
outputGrubbAscii B   = "b"
outputGrubbAscii D   = "d"
outputGrubbAscii PY  = "p'"
outputGrubbAscii TY  = "t'"
outputGrubbAscii TS  = "ts"
outputGrubbAscii TL  = "tl"
outputGrubbAscii DZ  = "dz"
outputGrubbAscii DL  = "dl"
outputGrubbAscii TSY = "ts'" -- note this
outputGrubbAscii TLY = "tl'" -- note this
outputGrubbAscii S   = "s"
outputGrubbAscii LH  = "lh"
outputGrubbAscii L   = "l"
outputGrubbAscii LY  = "l'"
outputGrubbAscii J   = "y"
outputGrubbAscii JY  = "y'"
outputGrubbAscii K   = "k"
outputGrubbAscii KW  = "kw"
outputGrubbAscii G   = "g"
outputGrubbAscii GW  = "gw"
outputGrubbAscii KY  = "k'"
outputGrubbAscii KWY = "kw'"
outputGrubbAscii Q   = "kh"
outputGrubbAscii QW  = "khw"
outputGrubbAscii GU  = "gh"
outputGrubbAscii GUW = "ghw"
outputGrubbAscii QY  = "kh'"
outputGrubbAscii QWY = "khw'"
outputGrubbAscii X   = "x"
outputGrubbAscii XW  = "xw"
outputGrubbAscii XU  = "xh"
outputGrubbAscii XUW = "xhw"
outputGrubbAscii W   = "w"
outputGrubbAscii WY  = "w'"
outputGrubbAscii Y   = "'"
outputGrubbAscii H   = "h"
outputGrubbAscii A   = "a"
outputGrubbAscii E   = "eh"
outputGrubbAscii I   = "i"
outputGrubbAscii O   = "o"
outputGrubbAscii U   = "u"
outputGrubbAscii AU  = "e"

outputGrubbAsciiJ :: KwakLetter -> T.Text
outputGrubbAsciiJ H = "j"
outputGrubbAsciiJ x = outputGrubbAscii x

-- | Output an upper-case Grubb-ASCII character.
outputGrubbAscii' :: KwakLetter -> T.Text
outputGrubbAscii' M   = "M"
outputGrubbAscii' MY  = "M'"
outputGrubbAscii' N   = "N"
outputGrubbAscii' NY  = "N'"
outputGrubbAscii' P   = "P"
outputGrubbAscii' T   = "T"
outputGrubbAscii' B   = "B"
outputGrubbAscii' D   = "D"
outputGrubbAscii' PY  = "P'"
outputGrubbAscii' TY  = "T'"
outputGrubbAscii' TS  = "Ts"
outputGrubbAscii' TL  = "Tl"
outputGrubbAscii' DZ  = "Dz"
outputGrubbAscii' DL  = "Dl"
outputGrubbAscii' TSY = "Ts'"
outputGrubbAscii' TLY = "Tl'"
outputGrubbAscii' S   = "S"
outputGrubbAscii' LH  = "Lh"
outputGrubbAscii' L   = "L"
outputGrubbAscii' LY  = "L'"
outputGrubbAscii' J   = "Y"
outputGrubbAscii' JY  = "Y'"
outputGrubbAscii' K   = "K"
outputGrubbAscii' KW  = "Kw"
outputGrubbAscii' G   = "G"
outputGrubbAscii' GW  = "Gw"
outputGrubbAscii' KY  = "K'"
outputGrubbAscii' KWY = "Kw'"
outputGrubbAscii' Q   = "Kh"
outputGrubbAscii' QW  = "Khw"
outputGrubbAscii' GU  = "Gh"
outputGrubbAscii' GUW = "Ghw"
outputGrubbAscii' QY  = "Kh'"
outputGrubbAscii' QWY = "Khw'"
outputGrubbAscii' X   = "X"
outputGrubbAscii' XW  = "Xw"
outputGrubbAscii' XU  = "Xh"
outputGrubbAscii' XUW = "Xhw"
outputGrubbAscii' W   = "W"
outputGrubbAscii' WY  = "W'"
outputGrubbAscii' Y   = "'"
outputGrubbAscii' H   = "H"
outputGrubbAscii' A   = "A"
outputGrubbAscii' E   = "Eh"
outputGrubbAscii' I   = "I"
outputGrubbAscii' O   = "O"
outputGrubbAscii' U   = "U"
outputGrubbAscii' AU  = "E"

outputGrubbAsciiJ' :: KwakLetter -> T.Text
outputGrubbAsciiJ' H = "J"
outputGrubbAsciiJ' x = outputGrubbAscii' x

-- Strict Text-based output
decodeToGrubbAsciiOld :: [CasedChar] -> T.Text
decodeToGrubbAsciiOld = T.concat . (map $ mapChar $ mapCase outputGrubbAscii' outputGrubbAscii)

-- Again from U'mista

-- | This is the standard version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses strict `Text` output.
decodeToGrubbAscii :: [CasedChar] -> T.Text
decodeToGrubbAscii xs = decodeToGrubbMain $ setupGlottal xs

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses strict `Text` output.
decodeToGrubbAsciiX :: [CasedChar] -> T.Text
decodeToGrubbAsciiX xs = decodeToGrubbMain xs

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses strict `Text` output.
decodeToGrubbAsciiJ :: [CasedChar] -> T.Text
decodeToGrubbAsciiJ xs = decodeToGrubbMainJ $ setupGlottal xs

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses strict `Text` output.
decodeToGrubbAsciiJX :: [CasedChar] -> T.Text
decodeToGrubbAsciiJX xs = decodeToGrubbMain xs


decodeToGrubbMain :: [CasedChar] -> T.Text
decodeToGrubbMain = T.concat . (map $ mapChar $ mapCase outputGrubbAscii' outputGrubbAscii)

decodeToGrubbMainJ :: [CasedChar] -> T.Text
decodeToGrubbMainJ = T.concat . (map $ mapChar $ mapCase outputGrubbAsciiJ' outputGrubbAsciiJ)


-- left-fold
decodeToGrubbAscii' :: [[CasedChar]] -> [[CasedChar]] -> [CasedChar]
decodeToGrubbAscii' acc [] = concat $ reverse acc
decodeToGrubbAscii' acc ((x@(Kwak z1) : y@(Kwak z2) : xs) : xss)
    | (isCharLetter Y x) && (isKwkVow' z2) = ((decodeToGrubbAscii' ((  y:xs):acc) xss))
    | otherwise                            = ((decodeToGrubbAscii' ((x:y:xs):acc) xss))
decodeToGrubbAscii' acc (xs : xss) = (decodeToGrubbAscii' (xs:acc) xss)

-- Set up glottal stops.
setupGlottal :: [CasedChar] -> [CasedChar]
setupGlottal xs = decodeToGrubbAscii' [] $ groupBy isSameCaseType xs

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputGrubbAscii2 :: KwakLetter -> TL.Builder
outputGrubbAscii2 M   = "m"
outputGrubbAscii2 MY  = "m'"
outputGrubbAscii2 N   = "n"
outputGrubbAscii2 NY  = "n'"
outputGrubbAscii2 P   = "p"
outputGrubbAscii2 T   = "t"
outputGrubbAscii2 B   = "b"
outputGrubbAscii2 D   = "d"
outputGrubbAscii2 PY  = "p'"
outputGrubbAscii2 TY  = "t'"
outputGrubbAscii2 TS  = "ts"
outputGrubbAscii2 TL  = "tl"
outputGrubbAscii2 DZ  = "dz"
outputGrubbAscii2 DL  = "dl"
outputGrubbAscii2 TSY = "ts'" -- note this
outputGrubbAscii2 TLY = "tl'" -- note this
outputGrubbAscii2 S   = "s"
outputGrubbAscii2 LH  = "lh"
outputGrubbAscii2 L   = "l"
outputGrubbAscii2 LY  = "l'"
outputGrubbAscii2 J   = "y"
outputGrubbAscii2 JY  = "y'"
outputGrubbAscii2 K   = "k"
outputGrubbAscii2 KW  = "kw"
outputGrubbAscii2 G   = "g"
outputGrubbAscii2 GW  = "gw"
outputGrubbAscii2 KY  = "k'"
outputGrubbAscii2 KWY = "kw'"
outputGrubbAscii2 Q   = "kh"
outputGrubbAscii2 QW  = "khw"
outputGrubbAscii2 GU  = "gh"
outputGrubbAscii2 GUW = "ghw"
outputGrubbAscii2 QY  = "kh'"
outputGrubbAscii2 QWY = "khw"
outputGrubbAscii2 X   = "x"
outputGrubbAscii2 XW  = "xw"
outputGrubbAscii2 XU  = "xh"
outputGrubbAscii2 XUW = "xhw"
outputGrubbAscii2 W   = "w"
outputGrubbAscii2 WY  = "w'"
outputGrubbAscii2 Y   = "'"
outputGrubbAscii2 H   = "h"
outputGrubbAscii2 A   = "a"
outputGrubbAscii2 E   = "eh"
outputGrubbAscii2 I   = "i"
outputGrubbAscii2 O   = "o"
outputGrubbAscii2 U   = "u"
outputGrubbAscii2 AU  = "e"

outputGrubbAsciiJ2 :: KwakLetter -> TL.Builder
outputGrubbAsciiJ2 H = "j"
outputGrubbAsciiJ2 x = outputGrubbAscii2 x

outputGrubbAscii2' :: KwakLetter -> TL.Builder
outputGrubbAscii2' M   = "M"
outputGrubbAscii2' MY  = "M'"
outputGrubbAscii2' N   = "N"
outputGrubbAscii2' NY  = "N'"
outputGrubbAscii2' P   = "P"
outputGrubbAscii2' T   = "T"
outputGrubbAscii2' B   = "B"
outputGrubbAscii2' D   = "D"
outputGrubbAscii2' PY  = "P'"
outputGrubbAscii2' TY  = "T'"
outputGrubbAscii2' TS  = "Ts"
outputGrubbAscii2' TL  = "Tl"
outputGrubbAscii2' DZ  = "Dz"
outputGrubbAscii2' DL  = "Dl"
outputGrubbAscii2' TSY = "Ts'"
outputGrubbAscii2' TLY = "Tl'"
outputGrubbAscii2' S   = "S"
outputGrubbAscii2' LH  = "Lh"
outputGrubbAscii2' L   = "L"
outputGrubbAscii2' LY  = "L'"
outputGrubbAscii2' J   = "Y"
outputGrubbAscii2' JY  = "Y'"
outputGrubbAscii2' K   = "K"
outputGrubbAscii2' KW  = "Kw"
outputGrubbAscii2' G   = "G"
outputGrubbAscii2' GW  = "Gw"
outputGrubbAscii2' KY  = "K'"
outputGrubbAscii2' KWY = "Kw'"
outputGrubbAscii2' Q   = "Kh"
outputGrubbAscii2' QW  = "Khw"
outputGrubbAscii2' GU  = "Gh"
outputGrubbAscii2' GUW = "Ghw"
outputGrubbAscii2' QY  = "Kh'"
outputGrubbAscii2' QWY = "Khw'"
outputGrubbAscii2' X   = "X"
outputGrubbAscii2' XW  = "Xw"
outputGrubbAscii2' XU  = "Xh"
outputGrubbAscii2' XUW = "Xhw"
outputGrubbAscii2' W   = "W"
outputGrubbAscii2' WY  = "W'"
outputGrubbAscii2' Y   = "'"
outputGrubbAscii2' H   = "H"
outputGrubbAscii2' A   = "A"
outputGrubbAscii2' E   = "Eh"
outputGrubbAscii2' I   = "I"
outputGrubbAscii2' O   = "O"
outputGrubbAscii2' U   = "U"
outputGrubbAscii2' AU  = "E"

outputGrubbAsciiJ2' :: KwakLetter -> TL.Builder
outputGrubbAsciiJ2' H = "J"
outputGrubbAsciiJ2' x = outputGrubbAscii2' x

-- | This is the standard version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGrubbAscii2 :: [CasedChar] -> T.Text
decodeToGrubbAscii2 = TL.toStrict . decodeToGrubbAsciiLazy

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGrubbAsciiX2 :: [CasedChar] -> T.Text
decodeToGrubbAsciiX2 = TL.toStrict . decodeToGrubbAsciiLazyX

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGrubbAsciiJ2 :: [CasedChar] -> T.Text
decodeToGrubbAsciiJ2 = TL.toStrict . decodeToGrubbAsciiLazyJ

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses strict `Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToGrubbAsciiJX2 :: [CasedChar] -> T.Text
decodeToGrubbAsciiJX2 = TL.toStrict . decodeToGrubbAsciiLazyJX

-- | This is the standard version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGrubbAsciiLazy :: [CasedChar] -> TL.Text
decodeToGrubbAsciiLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGrubbAscii2' outputGrubbAscii2)) . setupGlottal

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"H\/h\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGrubbAsciiLazyX :: [CasedChar] -> TL.Text
decodeToGrubbAsciiLazyX = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGrubbAscii2' outputGrubbAscii2))

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are ommited.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGrubbAsciiLazyJ :: [CasedChar] -> TL.Text
decodeToGrubbAsciiLazyJ = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGrubbAsciiJ2' outputGrubbAsciiJ2)) . setupGlottal

-- | This is an alternate version of Grubb-ASCII,
-- where \/h\/ is represented as \"J\/j\", and
-- glottal stops at the beginnings of words
-- are __not__ omitted.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToGrubbAsciiLazyJX :: [CasedChar] -> TL.Text
decodeToGrubbAsciiLazyJX = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGrubbAsciiJ2' outputGrubbAsciiJ2))


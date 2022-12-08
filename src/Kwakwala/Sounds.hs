{-|
Module      : Kwakwala.Sounds
Description : Kwak'wala phonology codes
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains the types that
are used internally to represent 
Kwak'wala phonemes. Note that there
are no stress markers, since most written
Kwak'wala lacks them.
-}

module Kwakwala.Sounds
    -- * Types
    ( KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    , CasedWord(..)
    -- * Functions
    -- ** Capitalisation Helpers
    , makeCase
    , stripCase
    , mapCase
    , isMaj
    , isMin
    -- ** Mapping Functions
    , mapChar
    , mapChar2
    -- ** Predicates
    , isKwkVow
    , isKwkVow'
    , isKwkVow''
    , isSameCaseType
    , isCharLetter
    , isKwakWord
    ) where

import qualified Data.Text as T

-- | The basic type representing sounds in Kwak'wala.
-- Some conventions:
--
--   * __Y__ stands in for an apostrophe, and having
--   one after a letter indicates glottalisation
--   or an ejective consonant.
--
--   * __U__ after a consonant indicates that it is
--   the "U"vular version of that consonant, with
--   the exception of voiceless uvular plosives,
--   which are represented by __Q__.
--
--   * Palatalisation of velar consonants is not 
--   notated, as is the case with most orthographies.
--
data KwakLetter
   = M  | MY | N  | NY
   | P  | T  | B  | D  | PY  | TY
   | TS | TL | DZ | DL | TSY | TLY
   | S  | LH
   | L  | LY | J  | JY
   | K  | KW | G  | GW  | KY | KWY
   | Q  | QW | GU | GUW | QY | QWY
   | X  | XW | XU | XUW
   | W  | WY
   | Y  | H
   | A | E | I | O | U | AU
   deriving (Show,Eq,Ord)

-- | A letter/sound together with a capitalisation marker.
-- This is used for text rather than for phonetic transcriptions,
-- since capitalisation conveys some semantic value.
data CasedLetter = Maj KwakLetter    | Min KwakLetter deriving (Show,Eq,Ord)

-- | Either a cased Kwak'wala letter, or some plaintext (usually punctuation).
data CasedChar = Kwak CasedLetter    | Punct  T.Text deriving (Show,Eq) 

-- | Either a sequence of `CasedLetter`s, or some plaintext (usually punctuation).
data CasedWord = KwakW [CasedLetter] | PunctW T.Text deriving (Show,Eq,Ord) -- will want to filter out words first

-- | Use a function to convert some `CasedChar` to `T.Text`,
-- or leave it alone if it's some punctuation.
mapChar :: (CasedLetter -> T.Text) -> CasedChar -> T.Text
mapChar f (Kwak  x) = f x
mapChar _ (Punct x) =   x

-- | A mapping from `CasedChar` to a target type.
mapChar2 :: (T.Text -> b)      -- ^ The mapping over punctuation/plaintext.
         -> (CasedLetter -> b) -- ^ The mapping over `CasedLetter`s.
         -> CasedChar          -- ^ The input.
         -> b                  -- ^ The output value.
mapChar2 f g (Kwak  x) = g x
mapChar2 f g (Punct x) = f x

-- | Convert a `KwakLetter` to a `CasedLetter`
-- depending on the value of a `Bool`.
makeCase :: Bool -> KwakLetter -> CasedLetter
makeCase True  x = Maj x
makeCase False x = Min x

-- | Remove the cased-ness from a `CasedLetter`.
stripCase :: CasedLetter -> KwakLetter
stripCase (Min x) = x
stripCase (Maj x) = x

-- | Convert a `CasedLetter` to another type,
-- using different functions depending on whether
-- the letter is upper or lower case.
mapCase :: (KwakLetter -> b) -> (KwakLetter -> b) -> CasedLetter -> b
mapCase f g (Maj x) = f x
mapCase f g (Min x) = g x

-- | Check whether a `CasedLetter` is upper-case.
isMaj :: CasedLetter -> Bool
isMaj (Maj _) = True
isMaj (Min _) = False

-- | Check whether a `CasedLetter` is lower-case.
isMin :: CasedLetter -> Bool
isMin (Min _) = True
isMin (Maj _) = False

-- | Check whether a `KwakLetter` is a vowel.
isKwkVow :: KwakLetter -> Bool
isKwkVow A  = True
isKwkVow E  = True
isKwkVow I  = True
isKwkVow O  = True
isKwkVow U  = True
isKwkVow AU = True
isKwkVow _  = False

-- | Check whether a `CasedLetter` is a vowel.
isKwkVow' :: CasedLetter -> Bool
isKwkVow' (Maj x) = isKwkVow x
isKwkVow' (Min x) = isKwkVow x

-- | Check wheter a `CasedChar` is a vowel.
isKwkVow'' :: CasedChar -> Bool
isKwkVow'' (Kwak x) = isKwkVow' x
isKwkVow'' _        = False

-- | Check whether two `CasedChar`s are both
-- letters, or both punctuation.
isSameCaseType :: CasedChar -> CasedChar -> Bool
isSameCaseType (Kwak _) (Kwak _) = True
isSameCaseType (Punct _) (Punct _) = True
isSameCaseType _ _ = False

-- | Check whether a `CasedChar` is the same
-- letter as a `KwakLetter`, ignoring capitalisation.
isCharLetter :: KwakLetter -> CasedChar -> Bool
isCharLetter x (Kwak (Min y)) = x == y
isCharLetter x (Kwak (Maj y)) = x == y
isCharLetter _ _              = False

-- | Check whether a `CasedWord` is a word.
isKwakWord :: CasedWord -> Bool
isKwakWord (KwakW _) = True
isKwakWord _         = False

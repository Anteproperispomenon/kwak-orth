module Test.Helpers 
  ( toMaj
  , toMin
  , toMaj'
  , toMin'
  ) where

import Kwakwala.Sounds

-- | Make a `CasedLetter` upper-case.
toMaj :: CasedLetter -> CasedLetter
toMaj (Min x) = Maj x
toMaj x = x

-- | Make a `CasedLetter` lower-case.
toMin :: CasedLetter -> CasedLetter
toMin (Maj x) = Min x
toMin x = x

-- | Make a `CasedChar` upper-case.
toMaj' :: CasedChar -> CasedChar
toMaj' (Kwak x) = Kwak $ toMaj x
toMaj' x = x

-- | Make a `CasedChar` lower-case.
toMin' :: CasedChar -> CasedChar
toMin' (Kwak x) = Kwak $ toMin x
toMin' x = x


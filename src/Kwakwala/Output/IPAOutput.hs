{-|
Module      : Kwakwala.Output.IPAOutput
Description : IPA output for Kwak'wala.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module has output functions for IPA
from Kwak'wala. IPA is not frequently used
to write Kwak'wala, so this is mainly used
to produce IPA pronunciations of Kwak'wala
text, usually for an academic audience not
well-versed in Kwak'wala.

Note that IPA has no casing, so upper and
lower case characters are treated the same.
-}

module Kwakwala.Output.IPAOutput
    -- * Exclusively Using Strict Text
    ( decodeToIpa
    , decodeToIpaAlt
    -- * Strict Text with Builders
    , decodeToIpa2
    , decodeToIpaAlt2
    -- * Lazy Text Output
    , decodeToIpaLazy
    , decodeToIpaLazyAlt
    ) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

import Control.Monad
import Data.Char
import Data.String

import Kwakwala.Sounds

-------------------------------------------
-- With ties between affricates.

-- Using the IPA to write Kwak'wala
outputIpaX :: (IsString a) => KwakLetter -> a
outputIpaX M   = "m"
outputIpaX MY  = "mˀ"
outputIpaX N   = "n"
outputIpaX NY  = "nˀ"
outputIpaX P   = "p"
outputIpaX T   = "t"
outputIpaX B   = "b"
outputIpaX D   = "d"
outputIpaX PY  = "pʼ"
outputIpaX TY  = "tʼ"
outputIpaX TS  = "t͡s"
outputIpaX TL  = "t͡ɬ"
outputIpaX DZ  = "d͡z"
outputIpaX DL  = "d͡ɮ"
outputIpaX TSY = "t͡sʼ"
outputIpaX TLY = "t͡ɬʼ"
outputIpaX S   = "s"
outputIpaX LH  = "ɬ"
outputIpaX L   = "l"
outputIpaX LY  = "lˀ"
outputIpaX J   = "j"
outputIpaX JY  = "jˀ"
outputIpaX K   = "kʲ"
outputIpaX KW  = "kʷ"
outputIpaX G   = "gʲ"
outputIpaX GW  = "gʷ"
outputIpaX KY  = "kʼ"
outputIpaX KWY = "kʷʼ"
outputIpaX Q   = "q"
outputIpaX QW  = "qʷ"
outputIpaX GU  = "ɢ"
outputIpaX GUW = "ɢʷ"
outputIpaX QY  = "qʼ"
outputIpaX QWY = "qʷʼ"
outputIpaX X   = "xʲ"
outputIpaX XW  = "xʷ"
outputIpaX XU  = "χ"
outputIpaX XUW = "χʷ"
outputIpaX W   = "w"
outputIpaX WY  = "wˀ"
outputIpaX Y   = "ʔ"
outputIpaX H   = "h"
outputIpaX A   = "a"
outputIpaX E   = "e"
outputIpaX I   = "i"
outputIpaX O   = "o"
outputIpaX U   = "u"
outputIpaX AU  = "ə"

-------------------------------------------
-- Without ties between affricates.

outputIpaX' :: (IsString a) => KwakLetter -> a
outputIpaX' M   = "m"
outputIpaX' MY  = "mˀ"
outputIpaX' N   = "n"
outputIpaX' NY  = "nˀ"
outputIpaX' P   = "p"
outputIpaX' T   = "t"
outputIpaX' B   = "b"
outputIpaX' D   = "d"
outputIpaX' PY  = "pʼ"
outputIpaX' TY  = "tʼ"
outputIpaX' TS  = "ts"
outputIpaX' TL  = "tɬ"
outputIpaX' DZ  = "dz"
outputIpaX' DL  = "dɮ"
outputIpaX' TSY = "tsʼ"
outputIpaX' TLY = "tɬʼ"
outputIpaX' S   = "s"
outputIpaX' LH  = "ɬ"
outputIpaX' L   = "l"
outputIpaX' LY  = "lˀ"
outputIpaX' J   = "j"
outputIpaX' JY  = "jˀ"
outputIpaX' K   = "kʲ"
outputIpaX' KW  = "kʷ"
outputIpaX' G   = "gʲ"
outputIpaX' GW  = "gʷ"
outputIpaX' KY  = "kʼ"
outputIpaX' KWY = "kʷʼ"
outputIpaX' Q   = "q"
outputIpaX' QW  = "qʷ"
outputIpaX' GU  = "ɢ"
outputIpaX' GUW = "ɢʷ"
outputIpaX' QY  = "qʼ"
outputIpaX' QWY = "qʷʼ"
outputIpaX' X   = "xʲ"
outputIpaX' XW  = "xʷ"
outputIpaX' XU  = "χ"
outputIpaX' XUW = "χʷ"
outputIpaX' W   = "w"
outputIpaX' WY  = "wˀ"
outputIpaX' Y   = "ʔ"
outputIpaX' H   = "h"
outputIpaX' A   = "a"
outputIpaX' E   = "e"
outputIpaX' I   = "i"
outputIpaX' O   = "o"
outputIpaX' U   = "u"
outputIpaX' AU  = "ə"


outputIpa :: KwakLetter -> T.Text
outputIpa = outputIpaX

outputIpa' :: KwakLetter -> T.Text
outputIpa' = outputIpaX'

-- | Standard IPA text, with ties for affricates.
--
-- This version uses strict `T.Text` output.
decodeToIpa :: [CasedChar] -> T.Text
decodeToIpa = T.concat . (map $ mapChar $ mapCase outputIpa outputIpa)

-- | IPA text, but without ties for affricates.
--
-- This version uses strict `T.Text` output.
decodeToIpaAlt :: [CasedChar] -> T.Text
decodeToIpaAlt = T.concat . (map $ mapChar $ mapCase outputIpa' outputIpa')

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputIpa2 :: KwakLetter -> TL.Builder
outputIpa2 = outputIpaX

outputIpa2' :: KwakLetter -> TL.Builder
outputIpa2' = outputIpaX'

-- | Standard IPA text, with ties for affricates.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToIpa2 :: [CasedChar] -> T.Text
decodeToIpa2 = TL.toStrict . decodeToIpaLazy

-- | IPA text, but without ties for affricates.
--
-- This version uses strict `T.Text` output with
-- lazy `TL.Builder`s as an intermediate.
decodeToIpaAlt2 :: [CasedChar] -> T.Text
decodeToIpaAlt2 = TL.toStrict . decodeToIpaLazyAlt

-- | Standard IPA text, with ties for affricates.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToIpaLazy :: [CasedChar] -> TL.Text
decodeToIpaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2 outputIpa2))

-- | IPA text, but without ties for affricates.
--
-- This version uses lazy `TL.Text` output using `TL.Builder`s.
decodeToIpaLazyAlt :: [CasedChar] -> TL.Text
decodeToIpaLazyAlt = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2' outputIpa2'))

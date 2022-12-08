{-# LANGUAGE OverloadedStrings #-}

module Kwakwala.Output.IPAOutput
    ( decodeToIpa
    , decodeToIpaAlt
    , decodeToIpaAlt2
    , KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    ) where
-- asdfzxcv

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

import Control.Monad
-- import Control.Applicative

-- import Data.Functor
-- import Data.List
import Data.Char
import Data.String

import Kwakwala.Sounds

-- import Data.Either

import System.IO

fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

-------------------------------------------
-- Using Standard Strict Text

-- Using the IPA to write Kwak'wala

-- this is probably the wrong way to do this
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
-- outputIpa Spc  = " "
-- asdfzxcv





















-- this is probably the wrong way to do this
-- outputIpaX' :: (IsString a) => KwakLetter -> a
-- outputIpaX' = outputIpaX
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
-- outputIpa Spc  = " "
-- asdfzxcv

----------------------
-- Possibilities for smallcaps L in uppercase

-- Ļ L-cedilla / L-comma
-- Ŀ L-middot
-- Ɫ L-tilde (also ɫ)
-- Ⱡ L-double stroke



outputIpa :: KwakLetter -> T.Text
outputIpa = outputIpaX

outputIpa' :: KwakLetter -> T.Text
outputIpa' = outputIpaX'


-- Strict Text-based output
decodeToIpa :: [CasedChar] -> T.Text
decodeToIpa = T.concat . (map $ mapChar $ mapCase outputIpa outputIpa)

decodeToIpaAlt :: [CasedChar] -> T.Text
decodeToIpaAlt = T.concat . (map $ mapChar $ mapCase outputIpa' outputIpa')

-- decodeToIpa :: [CasedChar] -> T.Text
-- decodeToIpa = decodeToNapa

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputIpa2 :: KwakLetter -> TL.Builder
outputIpa2 = outputIpaX

outputIpa2' :: KwakLetter -> TL.Builder
outputIpa2' = outputIpaX'

decodeToIpa2 :: [CasedChar] -> T.Text
decodeToIpa2 = TL.toStrict . decodeToIpaLazy -- TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2' outputIpa2))

decodeToIpaAlt2 :: [CasedChar] -> T.Text
decodeToIpaAlt2 = TL.toStrict . decodeToIpaLazy -- TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2' outputIpa2))

-- decodeToIpa2 :: [CasedChar] -> T.Text
-- decodeToIpa2 = decodeToNapa2

decodeToIpaLazy :: [CasedChar] -> TL.Text
decodeToIpaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2 outputIpa2))

decodeToIpaLazyAlt :: [CasedChar] -> TL.Text
decodeToIpaLazyAlt = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2' outputIpa2'))


-- decodeToIpaLazy :: [CasedChar] -> TL.Text
-- decodeToIpaLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputIpa2' outputIpa2))



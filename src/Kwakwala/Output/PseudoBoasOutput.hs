{-# LANGUAGE OverloadedStrings #-}

module Kwakwala.Output.PseudoBoasOutput
    ( decodeToPseudoBoas
    , decodeToPseudoBoas2
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

-------------------------------------------
-- Using Standard Strict Text

-- Seems to use same apostrophe
-- (U+0313) as U'mista.

-- this is probably the wrong way to do this
outputPseudoBoasX :: (IsString a) => KwakLetter -> a
outputPseudoBoasX M   = "m"
outputPseudoBoasX MY  = "ᵋm"
outputPseudoBoasX N   = "n"
outputPseudoBoasX NY  = "ᵋn"
outputPseudoBoasX P   = "p"
outputPseudoBoasX T   = "t"
outputPseudoBoasX B   = "b"
outputPseudoBoasX D   = "d"
outputPseudoBoasX PY  = "p!"
outputPseudoBoasX TY  = "t!"
outputPseudoBoasX TS  = "ts"
outputPseudoBoasX TL  = "ʟ"
outputPseudoBoasX DZ  = "dz"
outputPseudoBoasX DL  = "ʟ\x323" -- \x323 = combining dot below
outputPseudoBoasX TSY = "ts!"
outputPseudoBoasX TLY = "ʟ!"
outputPseudoBoasX S   = "s"
outputPseudoBoasX LH  = "ł"
outputPseudoBoasX L   = "l"
outputPseudoBoasX LY  = "ᵋl"
outputPseudoBoasX J   = "y"
outputPseudoBoasX JY  = "ᵋy"
outputPseudoBoasX K   = "k·"
outputPseudoBoasX KW  = "kᵘ"
outputPseudoBoasX G   = "g·"
outputPseudoBoasX GW  = "gᵘ"
outputPseudoBoasX KY  = "k·!"
outputPseudoBoasX KWY = "k!ᵘ"
outputPseudoBoasX Q   = "q"
outputPseudoBoasX QW  = "qᵘ"
outputPseudoBoasX GU  = "g\x323" -- "g̣"
outputPseudoBoasX GUW = "g\x323ᵘ" -- "g̣ᵘ"
outputPseudoBoasX QY  = "q!"
outputPseudoBoasX QWY = "q!ᵘ"
outputPseudoBoasX X   = "x·"
outputPseudoBoasX XW  = "x̣ᵘ"
outputPseudoBoasX XU  = "x"
outputPseudoBoasX XUW = "xᵘ"
outputPseudoBoasX W   = "w"
outputPseudoBoasX WY  = "ᵋw"
outputPseudoBoasX Y   = "ᵋ"
outputPseudoBoasX H   = "h"
outputPseudoBoasX A   = "a" -- a
outputPseudoBoasX E   = "ä" -- a umlaut
outputPseudoBoasX I   = "i" -- i
outputPseudoBoasX O   = "â" -- a hatchet
outputPseudoBoasX U   = "u" -- u
outputPseudoBoasX AU  = "ᴇ"
-- outputPseudoBoas Spc  = " "
-- asdfzxcv

-- this is probably the wrong way to do this
outputPseudoBoasX' :: (IsString a) => KwakLetter -> a
outputPseudoBoasX' M   = "M"
outputPseudoBoasX' MY  = "ᵋM"
outputPseudoBoasX' N   = "N"
outputPseudoBoasX' NY  = "ᵋN"
outputPseudoBoasX' P   = "P"
outputPseudoBoasX' T   = "T"
outputPseudoBoasX' B   = "B"
outputPseudoBoasX' D   = "D"
outputPseudoBoasX' PY  = "P!"
outputPseudoBoasX' TY  = "T!"
outputPseudoBoasX' TS  = "Ts"
outputPseudoBoasX' TL  = "Ⱡ"
outputPseudoBoasX' DZ  = "Dz"
outputPseudoBoasX' DL  = "Ⱡ\x323" -- \x323 = combining dot below -- Ⱡ
outputPseudoBoasX' TSY = "Ts!"
outputPseudoBoasX' TLY = "Ⱡ!"
outputPseudoBoasX' S   = "S"
outputPseudoBoasX' LH  = "Ł"
outputPseudoBoasX' L   = "L"
outputPseudoBoasX' LY  = "ᵋL"
outputPseudoBoasX' J   = "Y"
outputPseudoBoasX' JY  = "ᵋY"
outputPseudoBoasX' K   = "K·"
outputPseudoBoasX' KW  = "Kᵘ"
outputPseudoBoasX' G   = "G·"
outputPseudoBoasX' GW  = "Gᵘ"
outputPseudoBoasX' KY  = "K·!"
outputPseudoBoasX' KWY = "K!ᵘ"
outputPseudoBoasX' Q   = "Q"
outputPseudoBoasX' QW  = "Qᵘ"
outputPseudoBoasX' GU  = "G\x323" -- "g̣"
outputPseudoBoasX' GUW = "G\x323ᵘ" -- "g̣ᵘ"
outputPseudoBoasX' QY  = "Q!"
outputPseudoBoasX' QWY = "Q!ᵘ"
outputPseudoBoasX' X   = "X·"
outputPseudoBoasX' XW  = "X̣ᵘ"
outputPseudoBoasX' XU  = "X"
outputPseudoBoasX' XUW = "Xᵘ"
outputPseudoBoasX' W   = "W"
outputPseudoBoasX' WY  = "ᵋW"
outputPseudoBoasX' Y   = "ᵋ"
outputPseudoBoasX' H   = "H"
outputPseudoBoasX' A   = "A" -- a
outputPseudoBoasX' E   = "Ä" -- a umlaut
outputPseudoBoasX' I   = "I" -- i
outputPseudoBoasX' O   = "Â" -- a hatchet
outputPseudoBoasX' U   = "U" -- u
outputPseudoBoasX' AU  = "Û"
-- asdfzxcv

----------------------
-- Possibilities for smallcaps L in uppercase

-- Ļ L-cedilla / L-comma
-- Ŀ L-middot
-- Ɫ L-tilde (also ɫ)
-- Ⱡ L-double stroke

outputPseudoBoas :: KwakLetter -> T.Text
outputPseudoBoas = outputPseudoBoasX

outputPseudoBoas' :: KwakLetter -> T.Text
outputPseudoBoas' = outputPseudoBoasX'

{-
outputPseudoBoas' :: KwakLetter -> T.Text
outputPseudoBoas' M   = "M"
outputPseudoBoas' MY  = "M\x313"
outputPseudoBoas' N   = "N"
outputPseudoBoas' NY  = "N\x313"
outputPseudoBoas' P   = "P"
outputPseudoBoas' T   = "T"
outputPseudoBoas' B   = "B"
outputPseudoBoas' D   = "D"
outputPseudoBoas' PY  = "P\x313"
outputPseudoBoas' TY  = "T\x313"
outputPseudoBoas' TS  = "C"
outputPseudoBoas' TL  = "ƛ"
outputPseudoBoas' DZ  = "Dᶻ"
outputPseudoBoas' DL  = "Λ"
outputPseudoBoas' TSY = "C\x313"
outputPseudoBoas' TLY = "ƛ̓"
outputPseudoBoas' S   = "S"
outputPseudoBoas' LH  = "Ł"
outputPseudoBoas' L   = "L"
outputPseudoBoas' LY  = "L\x313"
outputPseudoBoas' J   = "Y"
outputPseudoBoas' JY  = "Y\x313"
outputPseudoBoas' K   = "K"
outputPseudoBoas' KW  = "Kʷ"
outputPseudoBoas' G   = "G"
outputPseudoBoas' GW  = "Gʷ"
outputPseudoBoas' KY  = "K\x313"
outputPseudoBoas' KWY = "K\x313ʷ"
outputPseudoBoas' Q   = "Q"
outputPseudoBoas' QW  = "Qʷ"
outputPseudoBoas' GU  = "Ǧ"
outputPseudoBoas' GUW = "Ǧʷ"
outputPseudoBoas' QY  = "Q\x313"
outputPseudoBoas' QWY = "Q\x313ʷ"
outputPseudoBoas' X   = "X"
outputPseudoBoas' XW  = "Xʷ"
outputPseudoBoas' XU  = "X\x30c"
outputPseudoBoas' XUW = "X\x30cʷ"
outputPseudoBoas' W   = "W"
outputPseudoBoas' WY  = "W\x313"
outputPseudoBoas' Y   = "ʔ"
outputPseudoBoas' H   = "H"
outputPseudoBoas' A   = "A"
outputPseudoBoas' E   = "E"
outputPseudoBoas' I   = "I"
outputPseudoBoas' O   = "O"
outputPseudoBoas' U   = "U"
outputPseudoBoas' AU  = "Ə"
-- asdfzxcv
-}

-- Strict Text-based output
decodeToPseudoBoas :: [CasedChar] -> T.Text
decodeToPseudoBoas = T.concat . (map $ mapChar $ mapCase outputPseudoBoas' outputPseudoBoas)

-- decodeToPseudoBoas :: [CasedChar] -> T.Text
-- decodeToPseudoBoas = decodeToNapa

--------------------------------------------
-- Using Builders

-- Builder-based lower-case letter output
outputPseudoBoas2 :: KwakLetter -> TL.Builder
outputPseudoBoas2 = outputPseudoBoasX

outputPseudoBoas2' :: KwakLetter -> TL.Builder
outputPseudoBoas2' = outputPseudoBoasX'

{-
outputPseudoBoas2 :: KwakLetter -> TL.Builder
outputPseudoBoas2 M   = "m"
outputPseudoBoas2 MY  = "m\x313"
outputPseudoBoas2 N   = "n"
outputPseudoBoas2 NY  = "n\x313"
outputPseudoBoas2 P   = "p"
outputPseudoBoas2 T   = "t"
outputPseudoBoas2 B   = "b"
outputPseudoBoas2 D   = "d"
outputPseudoBoas2 PY  = "p\x313"
outputPseudoBoas2 TY  = "t\x313"
outputPseudoBoas2 TS  = "c"
outputPseudoBoas2 TL  = "ƛ"
outputPseudoBoas2 DZ  = "dᶻ"
outputPseudoBoas2 DL  = "λ"
outputPseudoBoas2 TSY = "c\x313"
outputPseudoBoas2 TLY = "ƛ\x313"
outputPseudoBoas2 S   = "s"
outputPseudoBoas2 LH  = "ł"
outputPseudoBoas2 L   = "l"
outputPseudoBoas2 LY  = "l\x313"
outputPseudoBoas2 J   = "y"
outputPseudoBoas2 JY  = "y\x313"
outputPseudoBoas2 K   = "k"
outputPseudoBoas2 KW  = "kʷ"
outputPseudoBoas2 G   = "g"
outputPseudoBoas2 GW  = "gʷ"
outputPseudoBoas2 KY  = "k\x313"
outputPseudoBoas2 KWY = "k\x313ʷ"
outputPseudoBoas2 Q   = "q"
outputPseudoBoas2 QW  = "qʷ"
outputPseudoBoas2 GU  = "ǧ"
outputPseudoBoas2 GUW = "ǧʷ"
outputPseudoBoas2 QY  = "q\x313"
outputPseudoBoas2 QWY = "q\x313ʷ"
outputPseudoBoas2 X   = "x"
outputPseudoBoas2 XW  = "xʷ"
outputPseudoBoas2 XU  = "x\x30c"
outputPseudoBoas2 XUW = "x\x30cʷ"
outputPseudoBoas2 W   = "w"
outputPseudoBoas2 WY  = "w\x313"
outputPseudoBoas2 Y   = "ʔ"
outputPseudoBoas2 H   = "h"
outputPseudoBoas2 A   = "a"
outputPseudoBoas2 E   = "e"
outputPseudoBoas2 I   = "i"
outputPseudoBoas2 O   = "o"
outputPseudoBoas2 U   = "u"
outputPseudoBoas2 AU  = "ə"
-- asdfzxcv

-- Builder-based Upper-case letters
outputPseudoBoas2' :: KwakLetter -> TL.Builder
outputPseudoBoas2' M   = "M"
outputPseudoBoas2' MY  = "M\x313"
outputPseudoBoas2' N   = "N"
outputPseudoBoas2' NY  = "N\x313"
outputPseudoBoas2' P   = "P"
outputPseudoBoas2' T   = "T"
outputPseudoBoas2' B   = "B"
outputPseudoBoas2' D   = "D"
outputPseudoBoas2' PY  = "P\x313"
outputPseudoBoas2' TY  = "T\x313"
outputPseudoBoas2' TS  = "C"
outputPseudoBoas2' TL  = "ƛ"
outputPseudoBoas2' DZ  = "Dᶻ"
outputPseudoBoas2' DL  = "Λ"
outputPseudoBoas2' TSY = "C\x313"
outputPseudoBoas2' TLY = "ƛ̓"
outputPseudoBoas2' S   = "S"
outputPseudoBoas2' LH  = "Ł"
outputPseudoBoas2' L   = "L"
outputPseudoBoas2' LY  = "L\x313"
outputPseudoBoas2' J   = "Y"
outputPseudoBoas2' JY  = "Y\x313"
outputPseudoBoas2' K   = "K"
outputPseudoBoas2' KW  = "Kʷ"
outputPseudoBoas2' G   = "G"
outputPseudoBoas2' GW  = "Gʷ"
outputPseudoBoas2' KY  = "K\x313"
outputPseudoBoas2' KWY = "K\x313ʷ"
outputPseudoBoas2' Q   = "Q"
outputPseudoBoas2' QW  = "Qʷ"
outputPseudoBoas2' GU  = "Ǧ"
outputPseudoBoas2' GUW = "Ǧʷ"
outputPseudoBoas2' QY  = "Q\x313"
outputPseudoBoas2' QWY = "Q\x313ʷ"
outputPseudoBoas2' X   = "X"
outputPseudoBoas2' XW  = "Xʷ"
outputPseudoBoas2' XU  = "X\x30c"
outputPseudoBoas2' XUW = "X\x30cʷ"
outputPseudoBoas2' W   = "W"
outputPseudoBoas2' WY  = "W\x313"
outputPseudoBoas2' Y   = "ʔ"
outputPseudoBoas2' H   = "H"
outputPseudoBoas2' A   = "A"
outputPseudoBoas2' E   = "E"
outputPseudoBoas2' I   = "I"
outputPseudoBoas2' O   = "O"
outputPseudoBoas2' U   = "U"
outputPseudoBoas2' AU  = "Ə"
-- asdfzxcv
-}

decodeToPseudoBoas2 :: [CasedChar] -> T.Text
decodeToPseudoBoas2 = TL.toStrict . decodeToPseudoBoasLazy -- TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputPseudoBoas2' outputPseudoBoas2))

-- decodeToPseudoBoas2 :: [CasedChar] -> T.Text
-- decodeToPseudoBoas2 = decodeToNapa2

decodeToPseudoBoasLazy :: [CasedChar] -> TL.Text
decodeToPseudoBoasLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputPseudoBoas2' outputPseudoBoas2))

-- decodeToPseudoBoasLazy :: [CasedChar] -> TL.Text
-- decodeToPseudoBoasLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputPseudoBoas2' outputPseudoBoas2))



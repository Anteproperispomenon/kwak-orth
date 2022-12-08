module Kwakwala.Output.GrubbAscii
    ( decodeToGrubbAscii
    , decodeToGrubbAscii2
    ) where

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T

import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Control.Monad

import Data.List
-- import Control.Applicative

-- import Data.Functor
-- import Data.List
import Data.Char

import Kwakwala.Sounds

-- import Data.Either

import System.IO

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
-- outputGrubbAscii Spc  = " "

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

-- Strict Text-based output
decodeToGrubbAsciiOld :: [CasedChar] -> T.Text
decodeToGrubbAsciiOld = T.concat . (map $ mapChar $ mapCase outputGrubbAscii' outputGrubbAscii)

-- Again from U'mista

-- Taking the initial glottal stop into account
decodeToGrubbAscii :: [CasedChar] -> T.Text
decodeToGrubbAscii xs = decodeToGrubbMain $ decodeToGrubbAscii' [] $ groupBy isSameCaseType xs

decodeToGrubbMain :: [CasedChar] -> T.Text
decodeToGrubbMain = T.concat . (map $ mapChar $ mapCase outputGrubbAscii' outputGrubbAscii)

-- left-fold
decodeToGrubbAscii' :: [[CasedChar]] -> [[CasedChar]] -> [CasedChar]
decodeToGrubbAscii' acc [] = concat $ reverse acc
decodeToGrubbAscii' acc ((x@(Kwak z1) : y@(Kwak z2) : xs) : xss)
    | (isCharLetter Y x) && (isKwkVow' z2) = ((decodeToGrubbAscii' ((  y:xs):acc) xss))
    | otherwise                            = ((decodeToGrubbAscii' ((x:y:xs):acc) xss))
decodeToGrubbAscii' acc (xs : xss) = (decodeToGrubbAscii' (xs:acc) xss)

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
-- outputGrubbAscii Spc  = " "

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

decodeToGrubbAscii2 :: [CasedChar] -> T.Text
decodeToGrubbAscii2 = TL.toStrict . decodeToGrubbAsciiLazy -- TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

-- decodeToNAPA2 :: [CasedChar] -> T.Text
-- decodeToNAPA2 = decodeToGrubbAscii2

decodeToGrubbAsciiLazy :: [CasedChar] -> TL.Text
decodeToGrubbAsciiLazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputGrubbAscii2' outputGrubbAscii2))

-- decodeToNAPALazy :: [CasedChar] -> TL.Text
-- decodeToNAPALazy = TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))

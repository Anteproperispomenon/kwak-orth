{-# LANGUAGE OverloadedStrings #-}

module Kwakwala.Parsers.UmistaParserNew
    ( KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    , encodeFromUmista
    , decodeToNAPA
    , decodeToNapa
    , decodeToNAPA2
    , decodeToNapa2
    , parseUmista
    ) where
-- asdfzxcv


import Data.Attoparsec.Text qualified as AT

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL

-- import qualified TextUTF8 as TU

import Control.Monad
import Control.Applicative

import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds

import Data.Either

import System.IO





fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8


-----------------------------------------
-- a̱

-- precombined : ǤǥḴḵ

-- Update when more possibilities are found
isUnderline :: Char -> Bool
isUnderline '̱' = True
-- isUnderline '̱' = True
-- isUnderline '_' = True (needed this when console was wrong
isUnderline _ = False

-- Apostrophe/Ejective Marker
isApost :: Char -> Bool
isApost '\'' = True
isApost '`'  = True
isApost '̕'  = True
isApost _ = False


-- ʷᵂ
isLabial :: Char -> Bool
isLabial 'w' = True
isLabial 'W' = True
isLabial 'ᵂ' = True
isLabial 'ʷ' = True
-- isLabial 'ᵂ' = True -- maybe redundant?
isLabial  _  = False

isW = isLabial

-----------------------------------------------------------

parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'k' || x == 'K')
    ; AT.peekChar >>= parseK' b
    }
-- asdfzxcv

-- x̱

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseQ  b
    | isApost     x = AT.anyChar >> AT.peekChar >>= parseKY b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseKW b
    | otherwise     = return $ makeCase b K
-- asdfzxcv

parseKUN :: AT.Parser CasedLetter
parseKUN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ḵ' || x == 'Ḵ')
    ; AT.peekChar >>= parseQ b
    }
-- asdfxzcv

parseKY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKY b Nothing = return $ makeCase b KY
parseKY b (Just x)
    | isW     x = AT.peekChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KY
-- awsdfzxcv

parseKW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKW b Nothing = return $ makeCase b KW
parseKW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KW
-- awsdfzxcv
-- 
-- Resultant Parser
parseQ :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQ b Nothing = return $ makeCase b Q
parseQ b (Just x)
    | isApost x = AT.anyChar >> AT.peekChar >>= parseQY b
    | isW     x = AT.anyChar >> AT.peekChar >>= parseQW b
    | otherwise = return $ makeCase b Q
-- asdfzxcv

parseQW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQW b Nothing = return $ makeCase b QW
parseQW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QW
-- asdfzxcv

parseQY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY b Nothing = return $ makeCase b QW
parseQY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY
-- asdfzxcv

------------------------------------------------------------------------

-- Ǥǥ

parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }
-- asdfzxcv

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseGU b
    | isW         x = AT.anyChar >> (return $ makeCase b GW)
    | otherwise     = return $ makeCase b G
-- asdfzxcv


parseGUN :: AT.Parser CasedLetter
parseGUN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ǥ' || x == 'Ǥ')
    ; AT.peekChar >>= parseGU b
    }
-- asdfxzcv

-- Resultant Parser
parseGU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseGU b Nothing = return $ makeCase b GU
parseGU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU
-- asdfzxcv

-----------------------------------------------------------------

parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'p' || x == 'P')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b PY)) else (return $ makeCase b P)
        ;       _  -> return $ makeCase b P
        }
    }
-- asdfzxcv

parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 't' || x == 'T')
    ; AT.peekChar >>= parseT' b
--  ; case x of
--      { (Just y) -> if isApost y then (AT.anyChar >> return TY) else return T
--      ;       _  -> return T
--      }
    }
-- asdfzxcv

-- ŁłƚǱǲǳɫɬ

parseT' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseT' b Nothing = return $ makeCase b T
parseT' b (Just x)
    | (isApost x)        = AT.anyChar >> AT.peekChar >>= parseTY b
    | (x == 's' || x == 'S') = AT.anyChar >> AT.peekChar >>= parseTS b
    | (x == 'l' || x == 'L' || x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ' || x == 'Ł') = AT.anyChar >> AT.peekChar >>= parseTL b
    | otherwise = return $ makeCase b T
-- asdfzxcv

parseTS' :: AT.Parser CasedLetter
parseTS' = AT.char 'ʦ' >> AT.peekChar >>= parseTS False

parseTY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTY b Nothing = return $ makeCase b TY
parseTY b (Just x)
    | (x == 's' || x == 'S') = AT.anyChar >> (return $ makeCase b TSY)
    | (x == 'l' || x == 'L' || x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ' || x == 'Ł') = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = (return $ makeCase b TY)
-- asdfzxcv

parseTS :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTS b Nothing = return $ makeCase b TS
parseTS b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TSY)
    | otherwise = return $ makeCase b TS
-- asdfzxcv

parseTL :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL b Nothing = return $ makeCase b TS
parseTL b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = return $ makeCase b TL
-- asdfzxcv

--------------------------

parseM :: AT.Parser CasedLetter
parseM = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'm' || x == 'M')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b MY)) else (return $ makeCase b M)
        ;       _  -> return $ makeCase b M
        }
    }
-- asdfzxcv

parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'n' || x == 'N')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b NY)) else (return $ makeCase b N)
        ;       _  -> return $ makeCase b N
        }
    }
-- asdfzxcv

-- ŁłƚǱǲǳɫɬ

parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'l' || x == 'L')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b LY)) else (return $ makeCase b L)
        ;       _  -> return $ makeCase b L
        }
    }
-- asdfzxcv

parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'j' || x == 'J' || x == 'y' || x == 'Y')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b JY)) else (return $ makeCase b J)
        ;       _  -> return $ makeCase b J
        }
    }
-- asdfzxcv

parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'w' || x == 'W')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost y then (AT.anyChar >> (return $ makeCase b WY)) else (return $ makeCase b W)
        ;       _  -> return $ makeCase b W
        }
    }
-- asdfzxcv

-------------------------------------


parseD :: AT.Parser CasedLetter
parseD = do
    { x <- AT.satisfy (\x -> x == 'd' || x == 'D')
    ; AT.peekChar >>= parseD' (isUpper x)
    }
-- asdfzxcv

-- ŁłƚǱǲǳɫɬ

parseD' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseD' b Nothing = return $ makeCase b T
parseD' b (Just x)
--  | (isApost x)            = AT.anyChar >> AT.peekChar >>= parseTY
    | (x == 'z' || x == 'Z') = AT.anyChar >> (return $ makeCase b DZ)
    | (x == 'l' || x == 'L') = AT.anyChar >> (return $ makeCase b DL)
    | otherwise = return $ makeCase b D
-- asdfzxcv

-- maybe deal with Ǳǲǳ later

-----------------------------------------------------------

parseX :: AT.Parser CasedLetter
parseX = do
    { x <- AT.satisfy (\x -> x == 'x' || x == 'X')
    ; AT.peekChar >>= parseX' (isUpper x)
    }
-- asdfzxcv

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing  = return $ makeCase b X
parseX' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseXW b
    | otherwise     = return $ makeCase b X
-- asdfzxcv

{-
parseKUN :: AT.Parser CasedLetter
parseKUN = do
    { AT.satisfy (\x -> x == 'ḵ' || x == 'Ḵ')
    ; AT.peekChar >>= parseQ
    }
-- asdfxzcv
-}

parseXW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXW b Nothing = return $ makeCase b XW
parseXW b (Just x)
    | isUnderline x = AT.peekChar >> (return $ makeCase b XUW)
    | otherwise     = return $ makeCase b XW
-- awsdfzxcv

-- Resultant Parser
parseXU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU b Nothing = return $ makeCase b XU
parseXU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU
-- asdfzxcv

--------------------------------------------------------

-- || x == 'B'

parseB :: AT.Parser CasedLetter
parseB = (AT.char 'b' $> Min B) <|> (AT.char 'B' $> Maj B)

parseH :: AT.Parser CasedLetter
parseH = (AT.char 'h' $> Min H) <|> (AT.char 'H' $> Maj H)

parseS :: AT.Parser CasedLetter
parseS = (AT.char 's' $> Min S) <|> (AT.char 'S' $> Maj S)

-- ŁłƚǱǲǳɫɬ

parseLH :: AT.Parser CasedLetter
parseLH = ((AT.satisfy (\x -> x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ')) $> Min LH) <|> (AT.char 'Ł' $> Maj LH)

parseY :: AT.Parser CasedLetter
parseY = AT.satisfy isApost $> (Min Y)

--------------------------------------
-- Vowels

parseA :: AT.Parser CasedLetter
parseA = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'a' || x == 'A')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if (isUnderline y) then (AT.anyChar >> (return $ makeCase b AU)) else (return $ makeCase b A)
        ;       _  -> return $ makeCase b A
        }
    }
-- asdfzxcv
-- a̱
-- x̱

-- parseE :: AT.Parser CasedLetter
-- parseE = (AT.satisfy (\x -> x == 'e' || x == 'E')) $> E

parseE :: AT.Parser CasedLetter
parseE = (AT.char 'e' $> Min E) <|> (AT.char 'E' $> Maj E)

parseI :: AT.Parser CasedLetter
parseI = (AT.char 'i' $> Min I) <|> (AT.char 'I' $> Maj I)

parseO :: AT.Parser CasedLetter
parseO = (AT.char 'o' $> Min O) <|> (AT.char 'O' $> Maj O)

parseU :: AT.Parser CasedLetter
parseU = (AT.char 'u' $> Min U) <|> (AT.char 'U' $> Maj U)

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- parseSpace :: AT.Parser CasedLetter
-- parseSpace = AT.char ' ' $> Spc

parseUmistaLetter :: AT.Parser CasedLetter
parseUmistaLetter = AT.choice [parseA,parseE,parseI,parseO,parseU
--                            ,parseSpace
                              ,parseK,parseG,parseKUN,parseGUN,parseX
                              ,parseP,parseT,parseM,parseN
                              ,parseL,parseW,parseY,parseB,parseH
                              ,parseD,parseLH,parseJ,parseS
                              ,parseTS'
                              ]
-- asdfzxcv

parseUmistaChar :: AT.Parser CasedChar
parseUmistaChar = (Kwak <$> parseUmistaLetter) <|> (Punct <$> T.singleton <$> AT.anyChar)

parseUmista :: AT.Parser [CasedChar]
parseUmista = AT.many1 parseUmistaChar

outputNAPA :: KwakLetter -> T.Text
outputNAPA M  = "m"
outputNAPA MY = "m̓"
outputNAPA N  = "n"
outputNAPA NY = "n̓"
outputNAPA P  = "p"
outputNAPA T  = "t"
outputNAPA B  = "b"
outputNAPA D  = "d"
outputNAPA PY = "p̓"
outputNAPA TY = "t̓"
outputNAPA TS = "c"
outputNAPA TL = "ƛ"
outputNAPA DZ = "ʒ"
outputNAPA DL = "λ"
outputNAPA TSY = "c̓"
outputNAPA TLY = "ƛ̓"
outputNAPA S   = "s"
outputNAPA LH  = "ł"
outputNAPA L   = "l"
outputNAPA LY  = "l̓"
outputNAPA J   = "y"
outputNAPA JY  = "y̓"
outputNAPA K   = "k"
outputNAPA KW  = "kʷ"
outputNAPA G  = "g"
outputNAPA GW  = "gʷ"
outputNAPA KY  = "k̓"
outputNAPA KWY  = "k̓ʷ"
outputNAPA Q  = "q"
outputNAPA QW  = "qʷ"
outputNAPA GU  = "ġ"
outputNAPA GUW  = "ġʷ"
outputNAPA QY  = "q̓"
outputNAPA QWY  = "q̓ʷ"
outputNAPA X  = "x"
outputNAPA XW  = "xʷ"
outputNAPA XU  = "x̣"
outputNAPA XUW  = "x̣ʷ"
outputNAPA W  = "w"
outputNAPA WY = "w̓"
outputNAPA Y  = "ʔ"
outputNAPA H  = "h"
outputNAPA A  = "a"
outputNAPA E  = "e"
outputNAPA I  = "i"
outputNAPA O  = "o"
outputNAPA U  = "u"
outputNAPA AU  = "ə"
-- outputNAPA Spc  = " "
-- asdfzxcv

outputNAPA' :: KwakLetter -> T.Text
outputNAPA' M  = "M"
outputNAPA' MY = "M̓"
outputNAPA' N  = "N"
outputNAPA' NY = "N̓"
outputNAPA' P  = "P"
outputNAPA' T  = "T"
outputNAPA' B  = "B"
outputNAPA' D  = "D"
outputNAPA' PY = "P̓"
outputNAPA' TY = "T̓"
outputNAPA' TS = "C"
outputNAPA' TL = "ƛ"
outputNAPA' DZ = "Ʒ"
outputNAPA' DL = "Λ"
outputNAPA' TSY = "C̓"
outputNAPA' TLY = "ƛ̓"
outputNAPA' S   = "S"
outputNAPA' LH  = "ł"
outputNAPA' L   = "L"
outputNAPA' LY  = "L̓"
outputNAPA' J   = "Y"
outputNAPA' JY  = "Y̓"
outputNAPA' K   = "K"
outputNAPA' KW  = "Kʷ"
outputNAPA' G  = "G"
outputNAPA' GW  = "Gʷ"
outputNAPA' KY  = "K̓"
outputNAPA' KWY  = "K̓ʷ"
outputNAPA' Q  = "Q"
outputNAPA' QW  = "Qʷ"
outputNAPA' GU  = "Ġ"
outputNAPA' GUW  = "Ġʷ"
outputNAPA' QY  = "Q̓"
outputNAPA' QWY  = "Q̓ʷ"
outputNAPA' X  = "X"
outputNAPA' XW  = "Xʷ"
outputNAPA' XU  = "X̣"
outputNAPA' XUW  = "X̣ʷ"
outputNAPA' W  = "W"
outputNAPA' WY = "W̓"
outputNAPA' Y  = "ʔ"
outputNAPA' H  = "H"
outputNAPA' A  = "A"
outputNAPA' E  = "E"
outputNAPA' I  = "I"
outputNAPA' O  = "O"
outputNAPA' U  = "U"
outputNAPA' AU  = "Ə"
-- outputNAPA' Spc  = " "
-- asdfzxcv

encodeFromUmista :: T.Text -> [CasedChar]
encodeFromUmista txt = fromRight [] $ AT.parseOnly parseUmista txt

decodeToNAPA :: [CasedChar] -> T.Text
decodeToNAPA = T.concat . (map $ mapChar $ mapCase outputNAPA' outputNAPA)

decodeToNapa :: [CasedChar] -> T.Text
decodeToNapa = decodeToNAPA

-- Əə

convertNAPA = T.concat . (map $ mapChar $ mapCase outputNAPA' outputNAPA)

doNAPA x = T.putStrLn $ convertNAPA $ fromRight [] $ AT.parseOnly parseUmista x


sentence1 = "ga̱lsga̱lʦisux̱ da ḵwaḵ̕wanix̱" :: T.Text

-- return \$ makeCase b ([A-Z]*)
-- \(return \$ makeCase b $1\)

--------------------------------------------
-- Using Builders

outputNAPA2 :: KwakLetter -> TL.Builder
outputNAPA2 M  = "m"
outputNAPA2 MY = "m̓"
outputNAPA2 N  = "n"
outputNAPA2 NY = "n̓"
outputNAPA2 P  = "p"
outputNAPA2 T  = "t"
outputNAPA2 B  = "b"
outputNAPA2 D  = "d"
outputNAPA2 PY = "p̓"
outputNAPA2 TY = "t̓"
outputNAPA2 TS = "c"
outputNAPA2 TL = "ƛ"
outputNAPA2 DZ = "ʒ"
outputNAPA2 DL = "λ"
outputNAPA2 TSY = "c̓"
outputNAPA2 TLY = "ƛ̓"
outputNAPA2 S   = "s"
outputNAPA2 LH  = "ł"
outputNAPA2 L   = "l"
outputNAPA2 LY  = "l̓"
outputNAPA2 J   = "y"
outputNAPA2 JY  = "y̓"
outputNAPA2 K   = "k"
outputNAPA2 KW  = "kʷ"
outputNAPA2 G  = "g"
outputNAPA2 GW  = "gʷ"
outputNAPA2 KY  = "k̓"
outputNAPA2 KWY  = "k̓ʷ"
outputNAPA2 Q  = "q"
outputNAPA2 QW  = "qʷ"
outputNAPA2 GU  = "ġ"
outputNAPA2 GUW  = "ġʷ"
outputNAPA2 QY  = "q̓"
outputNAPA2 QWY  = "q̓ʷ"
outputNAPA2 X  = "x"
outputNAPA2 XW  = "xʷ"
outputNAPA2 XU  = "x̣"
outputNAPA2 XUW  = "x̣ʷ"
outputNAPA2 W  = "w"
outputNAPA2 WY = "w̓"
outputNAPA2 Y  = "ʔ"
outputNAPA2 H  = "h"
outputNAPA2 A  = "a"
outputNAPA2 E  = "e"
outputNAPA2 I  = "i"
outputNAPA2 O  = "o"
outputNAPA2 U  = "u"
outputNAPA2 AU  = "ə"
-- outputNAPA2 Spc  = " "
-- asdfzxcv

outputNAPA2' :: KwakLetter -> TL.Builder
outputNAPA2' M  = "M"
outputNAPA2' MY = "M̓"
outputNAPA2' N  = "N"
outputNAPA2' NY = "N̓"
outputNAPA2' P  = "P"
outputNAPA2' T  = "T"
outputNAPA2' B  = "B"
outputNAPA2' D  = "D"
outputNAPA2' PY = "P̓"
outputNAPA2' TY = "T̓"
outputNAPA2' TS = "C"
outputNAPA2' TL = "ƛ"
outputNAPA2' DZ = "Ʒ"
outputNAPA2' DL = "Λ"
outputNAPA2' TSY = "C̓"
outputNAPA2' TLY = "ƛ̓"
outputNAPA2' S   = "S"
outputNAPA2' LH  = "ł"
outputNAPA2' L   = "L"
outputNAPA2' LY  = "L̓"
outputNAPA2' J   = "Y"
outputNAPA2' JY  = "Y̓"
outputNAPA2' K   = "K"
outputNAPA2' KW  = "Kʷ"
outputNAPA2' G  = "G"
outputNAPA2' GW  = "Gʷ"
outputNAPA2' KY  = "K̓"
outputNAPA2' KWY  = "K̓ʷ"
outputNAPA2' Q  = "Q"
outputNAPA2' QW  = "Qʷ"
outputNAPA2' GU  = "Ġ"
outputNAPA2' GUW  = "Ġʷ"
outputNAPA2' QY  = "Q̓"
outputNAPA2' QWY  = "Q̓ʷ"
outputNAPA2' X  = "X"
outputNAPA2' XW  = "Xʷ"
outputNAPA2' XU  = "X̣"
outputNAPA2' XUW  = "X̣ʷ"
outputNAPA2' W  = "W"
outputNAPA2' WY = "W̓"
outputNAPA2' Y  = "ʔ"
outputNAPA2' H  = "H"
outputNAPA2' A  = "A"
outputNAPA2' E  = "E"
outputNAPA2' I  = "I"
outputNAPA2' O  = "O"
outputNAPA2' U  = "U"
outputNAPA2' AU  = "Ə"

decodeToNapa2 = decodeToNAPA2

decodeToNAPA2 = TL.toStrict . TL.toLazyText . (mconcat . (map $ mapChar2 TL.fromText $ mapCase outputNAPA2' outputNAPA2))


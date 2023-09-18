{-|
Module      : Kwakwala.Parsers.Umista
Description : Parser for the Grubb-ASCII orthography
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This is the module for parsing the Grubb-ASCII
orthography. This orthography is a modified
version of the original Grubb orthography
(which is related to the U'mista orthography),
that has been changed to make it encodeable in
pure ASCII. This makes it useful for applications
where using non-ASCII characters is considerably
more difficult or even impossible.

Inspired by Grubb's usage of "eh" to represent
/a/ and ASCII encodings of Esperanto, characters
that are usually written with diacritics are
instead written with an "h" following them. In
newer versions, the "h" sound is instead written
as "j" to prevent overlap/clashes.
-}

module Kwakwala.Parsers.GrubbAsciiParser
    -- * Newer Parsers
    ( encodeFromGrubbAscii
    , parseGrubbAscii
    -- * Deprecated Parsers
    , encodeFromGrubbAsciiOld
    , parseGrubbAsciiOld
    ) where

import Data.Attoparsec.Text qualified as AT

import Data.Text qualified as T

import Control.Monad
import Control.Applicative

import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds
import Kwakwala.Parsers.Helpers

import Data.Either

-- These aren't really necessary;
-- They're just extras
-- Apostrophe/Ejective Marker
isApost :: Char -> Bool
isApost '\'' = True
isApost '`'  = True
isApost '̕'  = True
isApost '\x313' = True
isApost '\x2bc' = True
-- isApost '7' = True -- Careful with this one.
isApost _ = False

isApost' :: Char -> Bool
isApost' '\'' = True
isApost' '`'  = True
isApost' '̕'  = True
isApost' '\x313' = True
isApost' '\x2bc' = True
isApost' '7' = True -- Careful with this one.
isApost' _ = False

-- ʷᵂ
isLabial :: Char -> Bool
isLabial 'w' = True
isLabial 'W' = True
isLabial 'ᵂ' = True
isLabial 'ʷ' = True
isLabial  _  = False

isW :: Char -> Bool
isW = isLabial

-- To allow for more possibilities
-- Note: this is only for the 'h'
-- that is used as a modifier. It
-- is NOT used for the /h/ phoneme.
isH :: Char -> Bool
isH 'h' = True
isH 'H' = True
isH  _  = False

---------------------------------------------------------------
-- Parsing K

-----------------------
-- Entry Point
parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'k' || x == 'K')
    ; AT.peekChar >>= parseK' b
    }

parseQ :: AT.Parser CasedLetter
parseQ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'q' || x == 'Q')
    ; AT.peekChar >>= parseKH b
    }

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isApost'  x = AT.anyChar >> AT.peekChar >>= parseKY b
    | isW       x = AT.anyChar >> AT.peekChar >>= parseKW b
    | isH       x = AT.anyChar >> AT.peekChar >>= parseKH b
    | otherwise   = return $ makeCase b K

parseKH :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKH b Nothing = return $ makeCase b Q
parseKH b (Just x)
    | isApost' x = AT.anyChar >> AT.peekChar >>= parseKHY b
    | isW      x = AT.anyChar >> AT.peekChar >>= parseKHW b
    | otherwise  = return $ makeCase b Q

parseKY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKY b Nothing = return $ makeCase b KY
parseKY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b KWY)
    | isH     x = AT.anyChar >> AT.peekChar >>= parseKHY b
    | otherwise = return $ makeCase b KY

parseKHY :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKHY b Nothing = return $ makeCase b QY
parseKHY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY

parseKW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKW b Nothing = return $ makeCase b KW
parseKW b (Just x)
    | isApost' x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise  = return $ makeCase b KW

parseKHW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKHW b Nothing = return $ makeCase b KW
parseKHW b (Just x)
    | isApost' x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise  = return $ makeCase b QW

-----------------------
-- Entry Point For G
parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isH     x = AT.anyChar >> AT.peekChar >>= parseGH b
    | isW     x = AT.anyChar >> AT.peekChar >>= parseGW b -- (return $ makeCase b GW) -- AT.peekChar >>= parseGW b
    | otherwise = return $ makeCase b G

parseGH :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGH b Nothing = return $ makeCase b GU
parseGH b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU

parseGW :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGW b Nothing = return $ makeCase b GW
parseGW b (Just x)
    | isH     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GW

---------------------------------------------------------------
-- Parsing X

-----------------------
-- Entry Point
parseX :: AT.Parser CasedLetter
parseX = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'x' || x == 'X')
    ; AT.peekChar >>= parseX' b
    }

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing = return $ makeCase b X
parseX' b (Just x)
    | isH     x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isW     x = AT.anyChar >> AT.peekChar >>= parseXW b  -- (return $ makeCase b XW) -- AT.peekChar >>= parseXW b
    | otherwise = return $ makeCase b X

parseXU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU b Nothing = return $ makeCase b XU
parseXU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU

parseXW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXW b Nothing = return $ makeCase b XW
parseXW b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XW

-----------------------
-- Entry Point
parseD :: AT.Parser CasedLetter
parseD = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'd' || x == 'D')
    ; AT.peekChar >>= parseD' b
    }

-- ᶻ
parseD' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseD' b Nothing = return $ makeCase b D
parseD' b (Just x)
    | (x == 'z' || x == 'Z' || x == 'ᶻ') = AT.anyChar >> (return $ makeCase b DZ)
    | (x == 'l' || x == 'L'            ) = AT.anyChar >> (return $ makeCase b DL)
    | otherwise                          = return $ makeCase b D

-----------------------
-- Entry Point
parseZ :: AT.Parser CasedLetter
parseZ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'z' || x == 'Z' || x == 'ǳ' || x == 'Ǳ' || x == 'ǲ')
    ; return $ makeCase b DZ
    }

---------------------------------------------------------------
-- Parsing B, P, T, C, and S

-----------------------
-- Entry Point
parseB :: AT.Parser CasedLetter
parseB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'b' || x == 'B')
    ; return $ makeCase b B
    }

-----------------------
-- Entry Point
parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'p' || x == 'P')
    ; AT.peekChar >>= parseP' b
    }

parseP' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseP' b Nothing = return $ makeCase b P
parseP' b (Just x)
    | isApost' x = AT.anyChar >> (return $ makeCase b PY)
    | otherwise  = return $ makeCase b P

-----------------------
-- Entry Point
parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 't' || x == 'T')
    ; AT.peekChar >>= parseT' b
    }

parseT' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseT' b Nothing = return $ makeCase b T
parseT' b (Just x)
    | isApost' x             = AT.anyChar >> AT.peekChar >>= parseTY b -- (return $ makeCase b TY)
    | (x == 's' || x == 'S') = AT.anyChar >> AT.peekChar >>= parseTS b
    | (x == 'l' || x == 'L') = AT.anyChar >> AT.peekChar >>= parseTL b
    | otherwise              = return $ makeCase b T

parseTS :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTS b Nothing = return $ makeCase b TS
parseTS b (Just x)
    | isApost' x = AT.anyChar >> (return $ makeCase b TSY)
    | otherwise  = return $ makeCase b TS

parseTL :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL b Nothing = return $ makeCase b TL
parseTL b (Just x)
    | isApost' x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise  = return $ makeCase b TL

parseTY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTY b Nothing = return $ makeCase b TY
parseTY b (Just x)
    | (x == 's' || x == 'S') = AT.anyChar >> (return $ makeCase b TSY)
    | (x == 'l' || x == 'L') = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise              = return $ makeCase b TY

-----------------------
-- Entry Point
-- (Equivalent to Ts)
parseC :: AT.Parser CasedLetter
parseC = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'c' || x == 'C')
    ; AT.peekChar >>= parseTS b
    }

---------------------------------------------------------------
-- Parsing M and N

-----------------------
-- Entry Point
parseM :: AT.Parser CasedLetter
parseM = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'm' || x == 'M')
    ; AT.peekChar >>= parseM' b
    }

parseM' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseM' b Nothing = return $ makeCase b M
parseM' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b MY)
    | otherwise = return $ makeCase b M

parseMonly :: AT.Parser CasedLetter
parseMonly = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'm' || x == 'M')
    ; return $ makeCase b M
    }

-----------------------
-- Entry Point
parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'n' || x == 'N')
    ; AT.peekChar >>= parseN' b
    }

parseN' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseN' b Nothing = return $ makeCase b N
parseN' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b NY)
    | otherwise = return $ makeCase b N

parseNonly :: AT.Parser CasedLetter
parseNonly = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'n' || x == 'N')
    ; return $ makeCase b N
    }

---------------------------------------------------------------
-- Parsing J/Y, L, LH, and W

-----------------------
-- Entry Point
parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'y' || x == 'Y')
    ; AT.peekChar >>= parseJ' b
    }

parseJ' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseJ' b Nothing = return $ makeCase b J
parseJ' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b JY)
    | otherwise = return $ makeCase b J

parseJonly :: AT.Parser CasedLetter
parseJonly = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'y' || x == 'Y')
    ; return $ makeCase b J
    }


-----------------------
-- Entry Point
parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'l' || x == 'L')
    ; AT.peekChar >>= parseL' b
    }

parseL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseL' b Nothing = return $ makeCase b L
parseL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b LY)
    | isH     x = AT.anyChar >> (return $ makeCase b LH)
    | otherwise = return $ makeCase b L

parseLonly :: AT.Parser CasedLetter
parseLonly = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'l' || x == 'L')
    ; AT.peekChar >>= parseLonly' b
    }

parseLonly' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseLonly' b Nothing = return $ makeCase b L
parseLonly' b (Just x)
    | isH     x = AT.anyChar >> (return $ makeCase b LH)
    | otherwise = return $ makeCase b L

-----------------------
-- Entry Point
parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'w' || x == 'W')
    ; AT.peekChar >>= parseW' b
    }

parseW' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseW' b Nothing = return $ makeCase b W
parseW' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b WY)
    | otherwise = return $ makeCase b W

parseWonly :: AT.Parser CasedLetter
parseWonly = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'w' || x == 'W')
    ; return $ makeCase b W
    }


-- Taken from U'mista parser, again
parseY :: AT.Parser CasedLetter
parseY = AT.satisfy isApost >> AT.peekChar >>= parseY'

parseY' :: (Maybe Char) -> AT.Parser CasedLetter
parseY' Nothing = return (Min Y)
parseY' (Just x)
    | (x == 'm' || x == 'M') = AT.anyChar >> (return $ makeCase b MY)
    | (x == 'n' || x == 'N') = AT.anyChar >> (return $ makeCase b NY)
    | (x == 'y' || x == 'Y') = AT.anyChar >> (return $ makeCase b JY)
    | (x == 'w' || x == 'W') = AT.anyChar >> (return $ makeCase b WY)
    | (x == 'l' || x == 'L') = do
        -- If the following character is an H, it should
        -- be interpretted as Y + LH, not LY + H.
        { AT.anyChar 
        ; y <- AT.peekChar
        ; when (liftP isH y) (fail "\"'lh\" error")
        ; return $ makeCase b LY
        } <|> (return $ makeCase b Y)
    | otherwise              = return $ makeCase b Y -- i.e., the glottal stop takes on the case of the following letter
    where b = isUpper x

----------------------------------------------------
-- Other stuff

-----------------------
-- Entry Point
parseS :: AT.Parser CasedLetter
parseS = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 's' || x == 'S')
    ; return $ makeCase b S
    }

parseH :: AT.Parser CasedLetter
parseH = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'h' || x == 'H' || x == 'j' || x == 'J')
    ; return $ makeCase b H
    }

----------------------------------------------------
-- Vowels

parseA  :: AT.Parser CasedLetter
parseA  = (AT.char 'a' $> Min  A) <|> (AT.char 'A' $> Maj  A)

parseE :: AT.Parser CasedLetter
parseE = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'e' || x == 'E')
    ; AT.peekChar >>= parseE' b
    }

parseE' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseE' b Nothing = return $ makeCase b AU
parseE' b (Just x)
    | isH     x = AT.anyChar >> (return $ makeCase b E)
    | otherwise = return $ makeCase b AU

parseI  :: AT.Parser CasedLetter
parseI  = (AT.char 'i' $> Min  I) <|> (AT.char 'I' $> Maj  I)

parseO  :: AT.Parser CasedLetter
parseO  = (AT.char 'o' $> Min  O) <|> (AT.char 'O' $> Maj  O)

parseU  :: AT.Parser CasedLetter
parseU  = (AT.char 'u' $> Min  U) <|> (AT.char 'U' $> Maj  U)

--------------------------------------------------
-- Taken from U'mista parser

parseGrubbLetterOld :: AT.Parser CasedLetter
parseGrubbLetterOld = 
    AT.choice [parseA,parseE,parseI,parseO,parseU
              ,parseK,parseQ,parseG,parseX
              ,parseP,parseT,parseM,parseN,parseC
              ,parseL,parseW,parseY,parseB
              ,parseD,parseJ,parseS
              ,parseZ,parseH
              ]

-- Disallowing glottal markers after sonorants.
parseGrubbLetter :: AT.Parser CasedLetter
parseGrubbLetter = 
    AT.choice [parseA,parseE,parseI,parseO,parseU
              ,parseK,parseQ,parseG,parseX
              ,parseP,parseT,parseMonly,parseNonly,parseC
              ,parseLonly,parseWonly,parseY,parseB
              ,parseD,parseJonly,parseS
              ,parseZ,parseH
              ]

parseGrubbWord :: AT.Parser [CasedLetter]
parseGrubbWord = parseGrubbLetter >>= parseGrubbWord'

parseGrubbWord' :: CasedLetter -> AT.Parser [CasedLetter]
parseGrubbWord' ltr
    | (isKwkVow' ltr) = ([caseOf ltr Y,ltr] ++) <$> many parseGrubbLetter
    | otherwise       = (ltr:)                  <$> many parseGrubbLetter
    where caseOf (Maj _) = Maj
          caseOf (Min _) = Min

parseGrubbWordOld :: AT.Parser [CasedLetter]
parseGrubbWordOld = parseGrubbLetterOld >>= parseGrubbWordOld'

parseGrubbWordOld' :: CasedLetter -> AT.Parser [CasedLetter]
parseGrubbWordOld' ltr
    | (isKwkVow' ltr) = ([caseOf ltr Y,ltr] ++) <$> many parseGrubbLetterOld
    | otherwise       = (ltr:)                  <$> many parseGrubbLetterOld
    where caseOf (Maj _) = Maj
          caseOf (Min _) = Min

parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x || (x == '|')))

parsePunctsA :: AT.Parser CasedWord
parsePunctsA = PunctW <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x))

parseGrubbChar :: AT.Parser CasedChar
parseGrubbChar = (Kwak <$> parseGrubbLetter) <|> parsePipe <|> (Punct <$> T.singleton <$> AT.anyChar)

parseGrubbMain :: AT.Parser [CasedChar]
parseGrubbMain = (map Kwak <$> parseGrubbWord) <|> ((:[]) <$> parsePipe) <|> ((:[]) <$> parsePuncts) <|> ((:[]) <$> Punct <$> T.singleton <$> AT.anyChar)

parseGrubbMainOld :: AT.Parser [CasedChar]
parseGrubbMainOld = (map Kwak <$> parseGrubbWordOld) <|> ((:[]) <$> parsePipe) <|> ((:[]) <$> parsePuncts) <|> ((:[]) <$> Punct <$> T.singleton <$> AT.anyChar)


-- | `AT.Parser` for newer Grubb-ASCII variants
--
-- Use this function together with functions
-- like `AT.parseOnly` if you want error messages.
-- Otherwise, just use `encodeFromGrubbAscii`.
--
-- Note that this doesn't work on variants of 
-- Grubb-ASCII where the /j/ phoneme (usually
-- written as "y") is written as "j".
parseGrubbAscii :: AT.Parser [CasedChar]
parseGrubbAscii = concat <$> AT.many1 parseGrubbMain

-- | Direct encoder for newer Grubb-ASCII variants.
--
-- Note that this doesn't work on variants of 
-- Grubb-ASCII where the /j/ phoneme (usually
-- written as "y") is written as "j".
--
-- Note that if the parser runs into any errors,
-- this just returns an empty list. If you want
-- error messages, use `parseGrubbAscii` together
-- with `AT.parseOnly` or other `AT.Parser` runners.
encodeFromGrubbAscii :: T.Text -> [CasedChar]
encodeFromGrubbAscii txt = fromRight [] $ AT.parseOnly parseGrubbAscii txt

-- | `AT.Parser` for older Grubb-ASCII variants
--
-- This parser interprets apostrophes after 
-- sonorants as glottalization markers. However,
-- this is incorrect behaviour, as glottal stops
-- following sonorants/nasals do exist. This
-- version will incorrectly interpret such
-- instances.
--
-- Use this function together with functions
-- like `AT.parseOnly` if you want error messages.
-- Otherwise, just use `encodeFromGrubbAscii`.
--
-- Note that this doesn't work on variants of 
-- Grubb-ASCII where the /j/ phoneme (usually
-- written as "y") is written as "j".
parseGrubbAsciiOld :: AT.Parser [CasedChar]
parseGrubbAsciiOld = concat <$> AT.many1 parseGrubbMainOld

-- | Direct encoder for older Grubb-ASCII variants.
--
-- This parser interprets apostrophes after 
-- sonorants as glottalization markers. However,
-- this is incorrect behaviour, as glottal stops
-- following sonorants/nasals do exist. This
-- version will incorrectly interpret such
-- instances.
--
-- Note that this doesn't work on variants of 
-- Grubb-ASCII where the /j/ phoneme (usually
-- written as "y") is written as "j".
--
-- Note that if the parser runs into any errors,
-- this just returns an empty list. If you want
-- error messages, use `parseGrubbAscii` together
-- with `AT.parseOnly` or other `AT.Parser` runners.
encodeFromGrubbAsciiOld :: T.Text -> [CasedChar]
encodeFromGrubbAsciiOld txt = fromRight [] $ AT.parseOnly parseGrubbAsciiOld txt




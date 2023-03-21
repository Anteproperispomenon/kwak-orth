{-|
Module      : Kwakwala.Parsers.Island
Description : Parser for the Island Font Orthography.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This is the module for parsing the "Southern"
or NAPA orthography, encoded for the "Island"
font.
-}

module Kwakwala.Parsers.Island
    -- * Direct Encoder
    ( encodeFromIsland
    , encodeFromIslandOld
    -- * Parser
    , parseIsland
    , parseIslandOld
    ) where

import Data.Attoparsec.Text qualified as AT

import Data.Text          qualified as T

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds
import Kwakwala.Parsers.Helpers

import Data.Either

-- Primarily copied over from the NAPA parser file.

-- Apostrophe/Ejective Marker
isApost :: Char -> Bool
isApost '{' = True
isApost _ = False

-- ʷᵂ
isLabial :: Char -> Bool
isLabial '#' = True
isLabial  _  = False

isW :: Char -> Bool
isW = isLabial

isWedge :: Char -> Bool
isWedge '}' = True
isWedge '^' = True
isWedge  _  = False

-- For characters that are used
-- as letters in "Island".
isOtherAlph :: Char -> Bool
isOtherAlph '[' = True
isOtherAlph ']' = True
isOtherAlph '{' = True
isOtherAlph '}' = True
isOtherAlph '%' = True
isOtherAlph '>' = True
isOtherAlph '+' = True
isOtherAlph '@' = True
isOtherAlph '#' = True
isOtherAlph '^' = True
isOtherAlph  _  = False

---------------------------------------------------------------
-- Parsing K

-----------------------
-- Entry Point
parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'k' || x == 'K')
    ; AT.peekChar >>= parseK' b
    }
-- asdfzxcv

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isApost x = AT.anyChar >> AT.peekChar >>= parseKY b
    | isW     x = AT.anyChar >> AT.peekChar >>= parseKW b
    | otherwise = return $ makeCase b K

parseKY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKY b Nothing = return $ makeCase b KY
parseKY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KY

parseKW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKW b Nothing = return $ makeCase b KW
parseKW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KW

---------------------------------------------------------------
-- Parsing Q

-----------------------
-- Entry Point
parseQ :: AT.Parser CasedLetter
parseQ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'q' || x == 'Q')
    ; AT.peekChar >>= parseQ' b
    }
-- asdfzxcv

parseQ' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQ' b Nothing = return $ makeCase b Q
parseQ' b (Just x)
    | isApost     x = AT.anyChar >> AT.peekChar >>= parseQY b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseQW b
    | otherwise     = return $ makeCase b Q

parseQY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY b Nothing = return $ makeCase b QY
parseQY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY

parseQW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQW b Nothing = return $ makeCase b QW
parseQW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QW

---------------------------------------------------------------
-- Parsing G

-----------------------
-- Entry Point
parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isWedge     x = AT.anyChar >> AT.peekChar >>= parseGU b
    | isW         x = AT.anyChar >> (return $ makeCase b GW) -- AT.peekChar >>= parseGW b
    | otherwise     = return $ makeCase b G

parseGU :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGU b Nothing = return $ makeCase b GU
parseGU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU
-- asfdzxcv

-----------------------
-- Entry Point
{-
parseGUB :: AT.Parser CasedLetter
parseGUB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ǧ' || x == 'Ǧ')
    ; AT.peekChar >>= parseGU b
    }
-- asdfzxc
-}

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
    | isWedge     x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isW         x = AT.anyChar >> (return $ makeCase b XW) -- AT.peekChar >>= parseXW b
    | otherwise     = return $ makeCase b X

parseXU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU b Nothing = return $ makeCase b XU
parseXU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU

---------------------------------------------------------------
-- Parsing D

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
    | (x == 'z' || x == 'Z' || x == '+') = AT.anyChar >> (return $ makeCase b DZ)
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
    | isApost x = AT.anyChar >> (return $ makeCase b PY)
    | otherwise = return $ makeCase b P

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
    | isApost x = AT.anyChar >> (return $ makeCase b TY)
    | otherwise = return $ makeCase b T

-----------------------
-- Entry Point
parseC :: AT.Parser CasedLetter
parseC = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'c' || x == 'C')
    ; AT.peekChar >>= parseC' b
    }

parseC' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseC' b Nothing = return $ makeCase b TS
parseC' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TSY)
    | otherwise = return $ makeCase b TS

-----------------------
-- Entry Point
parseS :: AT.Parser CasedLetter
parseS = (AT.char 's' $> Min S) <|> (AT.char 'S' $> Maj S)

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

---------------------------------------------------------------
-- Parsing J/Y, L, LH, and W

-----------------------
-- Entry Point
parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'y' || x == 'Y' || x == 'j' || x == 'J')
    ; AT.peekChar >>= parseJ' b
    }

parseJ' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseJ' b Nothing = return $ makeCase b J
parseJ' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b JY)
    | otherwise = return $ makeCase b J

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
    | otherwise = return $ makeCase b L

-----------------------
-- Entry Point
-- Taken directly from the Umista parser
-- (Then added alternative for upper-case tilde-L)
parseLH :: AT.Parser CasedLetter
parseLH = (AT.satisfy (\x -> x == '>')) $> (Min LH)
-- parseLH = ((AT.satisfy (\x -> x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ')) $> Min LH) <|> (AT.char 'Ł' $> Maj LH) <|> (AT.char 'Ɫ' $> Maj LH)
-- Ɫ == U+2C62 == \x2c62

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

---------------------------------------------------------------
-- Parsing λ and ƛ

-----------------------
-- Entry Point
parseDL :: AT.Parser CasedLetter
parseDL = (AT.char ']' $> Min DL)

-----------------------
-- Entry Point
parseTL :: AT.Parser CasedLetter
parseTL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == '[') -- (\x -> (toLower x) == '[')
    ; AT.peekChar >>= parseTL' b
    }

parseTL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL' b Nothing = return $ makeCase b TL
parseTL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = return $ makeCase b TL

---------------------------------------------------------------
-- Parsing ʔ and H

-----------------------
-- Entry Point
parseY :: AT.Parser CasedLetter
parseY = do
    { AT.char '%'
    ; b <- tstm isUpper <$> AT.peekChar
    ; return $ makeCase b Y
    }
    where tstm p Nothing  = False
          tstm p (Just x) = p x

-----------------------
-- Entry Point
parseH :: AT.Parser CasedLetter
parseH = (AT.char 'h' $> Min H) <|> (AT.char 'H' $> Maj H)

---------------------------------------------------------------
-- Parsing Vowels

parseA  :: AT.Parser CasedLetter
parseA  = (AT.char 'a' $> Min  A) <|> (AT.char 'A' $> Maj  A)

parseAU :: AT.Parser CasedLetter
parseAU = (AT.char '@' $> Min AU)

parseE  :: AT.Parser CasedLetter
parseE  = (AT.char 'e' $> Min  E) <|> (AT.char 'E' $> Maj  E)

parseI  :: AT.Parser CasedLetter
parseI  = (AT.char 'i' $> Min  I) <|> (AT.char 'I' $> Maj  I)

parseO  :: AT.Parser CasedLetter
parseO  = (AT.char 'o' $> Min  O) <|> (AT.char 'O' $> Maj  O)

parseU  :: AT.Parser CasedLetter
parseU  = (AT.char 'u' $> Min  U) <|> (AT.char 'U' $> Maj  U)

---------------------------------------------------------------
-- The Full Parser

parseIslandLetter :: AT.Parser CasedLetter
parseIslandLetter = AT.choice 
  [parseA,parseE,parseI,parseO,parseU,parseAU
  ,parseK,parseQ,parseG,parseX
  ,parseP,parseT,parseM,parseN
  ,parseL,parseW,parseY,parseB,parseH
  ,parseD,parseLH,parseJ,parseS
  ,parseZ,parseDL,parseTL
  ,parseC
  ]

-- Parse non-alphabetical and non-apostrophe characters
-- until next letter.
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x || isOtherAlph x || (x == '|')))

parseIslandChar :: AT.Parser CasedChar
parseIslandChar = (Kwak <$> parseIslandLetter) <|> parsePipe <|> (Punct <$> T.singleton <$> AT.anyChar)

parseIslandCharNew :: AT.Parser CasedChar
parseIslandCharNew = (Kwak <$> parseIslandLetter) <|> parsePipe <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

-- | `AT.Parser` for the Island font/NAPA orthography.
--
-- Use this function together with functions
-- like `AT.parseOnly` if you want error messages
-- or with `AT.parse` if you want incremental
-- input. Otherwise, just use `encodeFromNapa`.
parseIsland :: AT.Parser [CasedChar]
parseIsland = AT.many1 parseIslandCharNew

-- | Older version of `parseNapa`.
parseIslandOld :: AT.Parser [CasedChar]
parseIslandOld = AT.many1 parseIslandChar

-- | Direct encoder for the Island font/NAPA orthography.
--
-- Note that if the parser runs into any errors,
-- this just returns an empty list. If you want
-- error messages, use `parseNapa` together
-- with `AT.parseOnly` or other `AT.Parser` runners.
encodeFromIsland :: T.Text -> [CasedChar]
encodeFromIsland txt = fromRight [] $ AT.parseOnly parseIsland txt

-- | Older version of `encodeFromNapa`.
encodeFromIslandOld :: T.Text -> [CasedChar]
encodeFromIslandOld txt = fromRight [] $ AT.parseOnly parseIslandOld txt


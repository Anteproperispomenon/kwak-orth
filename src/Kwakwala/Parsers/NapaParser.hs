{-# LANGUAGE OverloadedStrings #-}

-- asdfzxcv

module Kwakwala.Parsers.NapaParser
    ( KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    , encodeFromNapa
    , parseNapa
    , encodeFromNAPA
    , parseNAPA
    , parseNapaOld
    , encodeFromNapaOld
    ) where
-- asdfzxcv

import Data.Attoparsec.Text qualified as AT

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T

-- import qualified Data.Text.Lazy         as TL
-- import qualified Data.Text.Lazy.Builder as TL

-- import qualified TextUTF8 as TU

import Control.Monad
import Control.Applicative

import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds
import Kwakwala.Parsers.Helpers

import Data.Either

import System.IO

fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

-- Apostrophe/Ejective Marker
isApost :: Char -> Bool
isApost '\'' = True
isApost '`'  = True
isApost '̕'  = True
isApost '\x313' = True
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

isWedge :: Char -> Bool
isWedge '\x30c' = True -- Proper wedge/caron/hacek
isWedge '\x306' = True -- breve symbol
isWedge _       = False

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
    | isApost     x = AT.anyChar >> AT.peekChar >>= parseKY b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseKW b
    | otherwise     = return $ makeCase b K
-- asdfzxcv


parseKY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKY b Nothing = return $ makeCase b KY
parseKY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KY
-- awsdfzxcv

parseKW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseKW b Nothing = return $ makeCase b KW
parseKW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise = return $ makeCase b KW
-- awsdfzxcv

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
-- asdfzxcv


parseQY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY b Nothing = return $ makeCase b QY
parseQY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY
-- awsdfzxcv

parseQW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQW b Nothing = return $ makeCase b QW
parseQW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QW
-- awsdfzxcv

---------------------------------------------------------------
-- Parsing G

-----------------------
-- Entry Point
parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }
-- asdfzxcv

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isWedge     x = AT.anyChar >> AT.peekChar >>= parseGU b
    | isW         x = AT.anyChar >> (return $ makeCase b GW) -- AT.peekChar >>= parseGW b
    | otherwise     = return $ makeCase b G
-- asdfzxcv

-- Ǧǧ
-- Ǧ = \x1e6
-- ǧ = \x1e7

-----------------------
-- Entry Point
parseGUB :: AT.Parser CasedLetter
parseGUB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ǧ' || x == 'Ǧ')
    ; AT.peekChar >>= parseGU b
    }
-- asdfzxc

parseGU :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGU b Nothing = return $ makeCase b GU
parseGU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU
-- asfdzxcv

---------------------------------------------------------------
-- Parsing X

-----------------------
-- Entry Point
parseX :: AT.Parser CasedLetter
parseX = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'x' || x == 'X')
    ; AT.peekChar >>= parseX' b
    }
-- asdfzxcv

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing = return $ makeCase b X
parseX' b (Just x)
    | isWedge     x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isW         x = AT.anyChar >> (return $ makeCase b XW) -- AT.peekChar >>= parseXW b
    | otherwise     = return $ makeCase b X
-- asdfzxcv

parseXU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU b Nothing = return $ makeCase b XU
parseXU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU
-- asdfzxcv

---------------------------------------------------------------
-- Parsing D

-----------------------
-- Entry Point
parseD :: AT.Parser CasedLetter
parseD = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'd' || x == 'D')
    ; AT.peekChar >>= parseD' b
    }
-- asdfzxcv

-- ᶻ

parseD' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseD' b Nothing = return $ makeCase b D
parseD' b (Just x)
    | (x == 'z' || x == 'Z' || x == 'ᶻ') = AT.anyChar >> (return $ makeCase b DZ)
    | otherwise                          = return $ makeCase b D
-- asdfzxcv

-----------------------
-- Entry Point
parseZ :: AT.Parser CasedLetter
parseZ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'z' || x == 'Z' || x == 'ǳ' || x == 'Ǳ' || x == 'ǲ')
    ; return $ makeCase b DZ
    }
-- asdfzxcv

---------------------------------------------------------------
-- Parsing B, P, T, C, and S

-----------------------
-- Entry Point
parseB :: AT.Parser CasedLetter
parseB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'b' || x == 'B')
    ; return $ makeCase b B
    }
-- asdfzxcv

-----------------------
-- Entry Point
parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'p' || x == 'P')
    ; AT.peekChar >>= parseP' b
    }
-- asfdzxcv

parseP' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseP' b Nothing = return $ makeCase b P
parseP' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b PY)
    | otherwise = return $ makeCase b P
-- asdfzxcv

-----------------------
-- Entry Point
parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 't' || x == 'T')
    ; AT.peekChar >>= parseT' b
    }
-- asfdzxcv

parseT' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseT' b Nothing = return $ makeCase b T
parseT' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TY)
    | otherwise = return $ makeCase b T
-- asdfzxcv

-----------------------
-- Entry Point
parseC :: AT.Parser CasedLetter
parseC = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'c' || x == 'C')
    ; AT.peekChar >>= parseC' b
    }
-- asfdzxcv

parseC' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseC' b Nothing = return $ makeCase b TS
parseC' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TSY)
    | otherwise = return $ makeCase b TS
-- asdfzxcv

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
-- asfdzxcv

parseM' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseM' b Nothing = return $ makeCase b M
parseM' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b MY)
    | otherwise = return $ makeCase b M
-- asdfzxcv

-----------------------
-- Entry Point
parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'n' || x == 'N')
    ; AT.peekChar >>= parseN' b
    }
-- asfdzxcv

parseN' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseN' b Nothing = return $ makeCase b N
parseN' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b NY)
    | otherwise = return $ makeCase b N
-- asdfzxcv



---------------------------------------------------------------
-- Parsing J/Y, L, LH, and W

-----------------------
-- Entry Point
parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'y' || x == 'Y' || x == 'j' || x == 'J')
    ; AT.peekChar >>= parseJ' b
    }
-- asfdzxcv

parseJ' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseJ' b Nothing = return $ makeCase b J
parseJ' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b JY)
    | otherwise = return $ makeCase b J
-- asdfzxcv

-----------------------
-- Entry Point
parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'l' || x == 'L')
    ; AT.peekChar >>= parseL' b
    }
-- asfdzxcv

parseL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseL' b Nothing = return $ makeCase b L
parseL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b LY)
    | otherwise = return $ makeCase b L
-- asdfzxcv

-----------------------
-- Entry Point
-- Taken directly from the Umista parser
-- (Then added alternative for upper-case tilde-L)
parseLH :: AT.Parser CasedLetter
parseLH = ((AT.satisfy (\x -> x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ')) $> Min LH) <|> (AT.char 'Ł' $> Maj LH) <|> (AT.char 'Ɫ' $> Maj LH)
-- Ɫ == U+2C62 == \x2c62

-----------------------
-- Entry Point
parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'w' || x == 'W')
    ; AT.peekChar >>= parseW' b
    }
-- asfdzxcv

parseW' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseW' b Nothing = return $ makeCase b W
parseW' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b WY)
    | otherwise = return $ makeCase b W
-- asdfzxcv

---------------------------------------------------------------
-- Parsing λ and ƛ

-----------------------
-- Entry Point
parseDL :: AT.Parser CasedLetter
parseDL = (AT.char 'λ' $> Min DL) <|> (AT.char 'Λ' $> Maj DL)

-----------------------
-- Entry Point
parseTL :: AT.Parser CasedLetter
parseTL = do
    { b <- isUpper <$> AT.satisfy (\x -> (toLower x) == 'ƛ')
    ; AT.peekChar >>= parseTL' b
    }
-- asfdzxcv

parseTL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL' b Nothing = return $ makeCase b TL
parseTL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = return $ makeCase b TL
-- asdfzxcv

---------------------------------------------------------------
-- Parsing ʔ and H

-----------------------
-- Entry Point
parseY :: AT.Parser CasedLetter
parseY = do
    { AT.char 'ʔ'
    ; b <- tstm isUpper <$> AT.peekChar
    ; return $ makeCase b Y
    }
    where tstm p Nothing  = False
          tstm p (Just x) = p x
-- asdfzxcv

-----------------------
-- Entry Point
parseH :: AT.Parser CasedLetter
parseH = (AT.char 'h' $> Min H) <|> (AT.char 'H' $> Maj H)

---------------------------------------------------------------
-- Parsing Vowels

-- Ə == \399 == \x18f
-- ə == \601 == \x259

-- I forgot to substitute the right vowels in... whoops...
parseA  :: AT.Parser CasedLetter
parseA  = (AT.char 'a' $> Min  A) <|> (AT.char 'A' $> Maj  A)

parseAU :: AT.Parser CasedLetter
parseAU = (AT.char 'ə' $> Min AU) <|> (AT.char 'Ə' $> Maj AU)

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

parseNapaLetter :: AT.Parser CasedLetter
parseNapaLetter = AT.choice [parseA,parseE,parseI,parseO,parseU,parseAU
                            ,parseK,parseQ,parseG,parseGUB,parseX
                            ,parseP,parseT,parseM,parseN
                            ,parseL,parseW,parseY,parseB,parseH
                            ,parseD,parseLH,parseJ,parseS
                            ,parseZ,parseDL,parseTL
                            ,parseC
                            ]
-- asdfzxcv

-- Parse non-alphabetical and non-apostrophe characters
-- until next Umista Char.
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x || (x == '|')))

-- asdfzsxcv
{-
parsePipe :: AT.Parser CasedChar
parsePipe = Punct <$> ((AT.satisfy isPipe) `comb1` (AT.takeWhile1 (not . isPipe)) `comb2` (AT.satisfy isPipe))
    where comb1 = liftM2 (T.cons)
          comb2 = liftM2 (T.snoc)
-- asdfzsxcv

isPipe :: Char -> Bool
isPipe '|' = True
isPipe '¦' = True
-- isPipe '|' = True
isPipe  _  = False
-}

parseNapaChar :: AT.Parser CasedChar
parseNapaChar = (Kwak <$> parseNapaLetter) <|> parsePipe <|> (Punct <$> T.singleton <$> AT.anyChar)

parseNapaCharNew :: AT.Parser CasedChar
parseNapaCharNew = (Kwak <$> parseNapaLetter) <|> parsePipe <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

parseNapa :: AT.Parser [CasedChar]
parseNapa = AT.many1 parseNapaCharNew

parseNapaOld :: AT.Parser [CasedChar]
parseNapaOld = AT.many1 parseNapaChar

encodeFromNapa :: T.Text -> [CasedChar]
encodeFromNapa txt = fromRight [] $ AT.parseOnly parseNapa txt

encodeFromNapaOld :: T.Text -> [CasedChar]
encodeFromNapaOld txt = fromRight [] $ AT.parseOnly parseNapaOld txt

parseNAPA :: AT.Parser [CasedChar]
parseNAPA = parseNapa

encodeFromNAPA :: T.Text -> [CasedChar]
encodeFromNAPA = encodeFromNapa









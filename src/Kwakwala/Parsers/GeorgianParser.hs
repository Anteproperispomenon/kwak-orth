{-|
Module      : Kwakwala.Parsers.GeorgianParser
Description : Parser for a Kwak'wala Orthography based on the Georgian Alphabet.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module has parsers for an
orthography for Kwak'wala based on the
Georgian alphabet. Since Georgian has
many ejective sounds with independent
letters, it is fairly easy to adapt the
alphabet to Kwak'wala.
-}

module Kwakwala.Parsers.GeorgianParser
    ( encodeFromGeorgian
    , parseGeorgian
    ) where

import Data.Text            qualified as T
import Data.Attoparsec.Text qualified as AT

import Control.Monad
import Control.Applicative

import Data.Either
import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds

isLabial :: Char -> Bool
isLabial 'ვ' = True
isLabial 'Ვ' = True
isLabial 'Ⴅ' = True
isLabial 'ჿ' = True
isLabial 'Ჿ' = True
isLabial  _  = False

isW :: Char -> Bool
isW = isLabial

isPalatal :: Char -> Bool
isPalatal 'ჾ' = True
isPalatal 'Ჾ' = True
isPalatal  _  = False

isJ :: Char -> Bool
isJ = isPalatal

---------------------------------------------------------------
-- Parsing K

-----------------------
-- Entry Point
parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ქ' || x == 'Ⴕ' || x == 'Ქ')
    ; AT.peekChar >>= parseK' b
    }

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b KW)
    | isJ         x = AT.anyChar >> (return $ makeCase b K )
    | otherwise     = return $ makeCase b K

parseKY :: AT.Parser CasedLetter
parseKY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'კ' || x == 'Ⴉ' || x == 'Კ')
    ; AT.peekChar >>= parseKY' b
    }

parseKY' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKY' b Nothing = return $ makeCase b KY
parseKY' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b KWY)
    | isJ         x = AT.anyChar >> (return $ makeCase b KY )
    | otherwise     = return $ makeCase b KY

---------------------------------------------------------------
-- Parsing Q

-----------------------
-- Entry Point
parseQ :: AT.Parser CasedLetter
parseQ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჴ' || x == 'Ⴤ' || x == 'Ჴ')
    ; AT.peekChar >>= parseQ' b
    }

parseQ' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQ' b Nothing = return $ makeCase b Q
parseQ' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b QW)
    | otherwise     = return $ makeCase b Q

parseQY :: AT.Parser CasedLetter
parseQY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ყ' || x == 'Ⴗ' || x == 'Ყ')
    ; AT.peekChar >>= parseQY' b
    }

parseQY' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY' b Nothing = return $ makeCase b QY
parseQY' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY

---------------------------------------------------------------
-- Parsing G

-----------------------
-- Entry Point
parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'გ' || x == 'Ⴂ' || x == 'Გ')
    ; AT.peekChar >>= parseG' b
    }

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b GW)
    | isJ         x = AT.anyChar >> (return $ makeCase b G )
    | otherwise     = return $ makeCase b G

-----------------------
-- Entry Point
parseGU :: AT.Parser CasedLetter
parseGU = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ღ' || x == 'Ⴖ' || x == 'Ღ')
    ; AT.peekChar >>= parseGU' b
    }

parseGU' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGU' b Nothing = return $ makeCase b GU
parseGU' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU

---------------------------------------------------------------
-- Parsing X

-----------------------
-- Entry Point
parseX :: AT.Parser CasedLetter
parseX = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'რ' || x == 'Ⴐ' || x == 'Რ')
    ; AT.peekChar >>= parseX' b
    }

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing = return $ makeCase b X
parseX' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b XW)
    | isJ         x = AT.anyChar >> (return $ makeCase b X )
    | otherwise     = return $ makeCase b X

parseXU :: AT.Parser CasedLetter
parseXU = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ხ' || x == 'Ⴞ' || x == 'Ხ')
    ; AT.peekChar >>= parseXU' b
    }

parseXU' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU' b Nothing = return $ makeCase b XU
parseXU' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU

---------------------------------------------------------------
-- Parsing D

-----------------------
-- Entry Point
parseD :: AT.Parser CasedLetter
parseD = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'დ' || x == 'Ⴃ' || x == 'Დ')
    ; return $ makeCase b D
    }

-----------------------
-- Entry Point
parseDZ :: AT.Parser CasedLetter
parseDZ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ძ' || x == 'Ⴛ' || x == 'Ძ')
    ; return $ makeCase b DZ
    }

---------------------------------------------------------------
-- Parsing B, P, T, C, and S

-----------------------
-- Entry Point
parseB :: AT.Parser CasedLetter
parseB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ბ' || x == 'Ⴁ' || x == 'Ბ')
    ; return $ makeCase b B
    }

-----------------------
-- Entry Point
parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ფ' || x == 'Ⴔ' || x == 'Ფ')
    ; return $ makeCase b P
    }

parsePY :: AT.Parser CasedLetter
parsePY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'პ' || x == 'Ⴎ' || x == 'Პ')
    ; return $ makeCase b PY
    }

-----------------------
-- Entry Point
parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'თ' || x == 'Ⴇ' || x == 'Თ')
    ; return $ makeCase b T
    }

parseTY :: AT.Parser CasedLetter
parseTY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ტ' || x == 'Ⴒ' || x == 'Ტ')
    ; return $ makeCase b TY
    }

-----------------------
-- Entry Point
parseTS :: AT.Parser CasedLetter
parseTS = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ც' || x == 'Ⴚ' || x == 'Ც')
    ; return $ makeCase b TS
    }

parseTSY :: AT.Parser CasedLetter
parseTSY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'წ' || x == 'Ⴜ' || x == 'Წ')
    ; return $ makeCase b TSY
    }

-----------------------
-- Entry Point
parseS :: AT.Parser CasedLetter
parseS = (AT.char 'ს' $> Min S) <|> (AT.char 'Ⴑ' $> Maj S) <|> (AT.char 'Ს' $> Maj S)

---------------------------------------------------------------
-- Parsing M and N

-----------------------
-- Entry Point
parseM :: AT.Parser CasedLetter
parseM = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'მ' || x == 'Ⴋ' || x == 'Მ')
    ; return $ makeCase b M
    }

-----------------------
-- Entry Point
parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ნ' || x == 'Ⴌ' || x == 'Ნ')
    ; return $ makeCase b N
    }

---------------------------------------------------------------
-- Parsing J/Y, L, LH, and W

-----------------------
-- Entry Point
parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჲ' || x == 'Ⴢ' || x == 'Ჲ')
    ; return $ makeCase b J
    }

-----------------------
-- Entry Point
parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ლ' || x == 'Ⴊ' || x == 'Ლ')
    ; return $ makeCase b L
    }

-----------------------
-- Entry Point
parseLH :: AT.Parser CasedLetter
parseLH = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'შ' || x == 'Ⴘ' || x == 'Შ')
    ; return $ makeCase b LH
    }

-----------------------
-- Entry Point
parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ვ' || x == 'Ⴅ' || x == 'Ვ')
    ; return $ makeCase b W
    }

---------------------------------------------------------------
-- Parsing λ and ƛ

-----------------------
-- Entry Point
parseDL :: AT.Parser CasedLetter
parseDL = (AT.char 'ჯ' $> Min DL) <|> (AT.char 'Ⴟ' $> Maj DL) <|> (AT.char 'Ჯ' $> Maj DL)

-----------------------
-- Entry Point
parseTL :: AT.Parser CasedLetter
parseTL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჩ' || x == 'Ⴙ' || x == 'Ჩ')
    ; return $ makeCase b TL
    }

parseTLY :: AT.Parser CasedLetter
parseTLY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჭ' || x == 'Ⴝ' || x == 'Ჭ')
    ; return $ makeCase b TLY
    }

---------------------------------------------------------------
-- Parsing ʔ and H

-----------------------
-- Entry Point

parseY :: AT.Parser CasedLetter
parseY = parseY1 <|> parseY2

parseY1 :: AT.Parser CasedLetter
parseY1 = AT.char 'ჸ' >> AT.peekChar >>= parseY'

parseY' :: (Maybe Char) -> AT.Parser CasedLetter
parseY' Nothing = return (Min Y)
parseY' (Just x)
    | (x == 'მ'  || x == 'Ⴋ' || x == 'Მ') = AT.anyChar >> (return $ makeCase b MY)
    | (x == 'ნ'  || x == 'Ⴌ' || x == 'Ნ') = AT.anyChar >> (return $ makeCase b NY)
    | (x == 'ლ' || x == 'Ⴊ' || x == 'Ლ') = AT.anyChar >> (return $ makeCase b LY)
    | (x == 'ჲ'  || x == 'Ⴢ' || x == 'Ჲ') = AT.anyChar >> (return $ makeCase b JY)
    | (x == 'ვ'  || x == 'Ⴅ' || x == 'Ვ') = AT.anyChar >> (return $ makeCase b WY)
    | otherwise = return $ makeCase b Y -- i.e., the glottal stop takes on the case of the following letter
    where b = isUpper x

parseY2 :: AT.Parser CasedLetter
parseY2 = AT.char 'Ჸ' >> AT.peekChar >>= parseY''

-- Since Mtavruli has a character for the
-- glottal stop, the glottal stop is the
-- capitalised character.
parseY'' :: (Maybe Char) -> AT.Parser CasedLetter
parseY'' Nothing = return (Maj Y)
parseY'' (Just x)
    | (x == 'მ'  || x == 'Ⴋ' || x == 'Მ') = AT.anyChar >> (return $ Maj MY)
    | (x == 'ნ'  || x == 'Ⴌ' || x == 'Ნ') = AT.anyChar >> (return $ Maj NY)
    | (x == 'ლ' || x == 'Ⴊ' || x == 'Ლ') = AT.anyChar >> (return $ Maj LY)
    | (x == 'ჲ'  || x == 'Ⴢ' || x == 'Ჲ') = AT.anyChar >> (return $ Maj JY)
    | (x == 'ვ'  || x == 'Ⴅ' || x == 'Ვ') = AT.anyChar >> (return $ Maj WY)
    | otherwise = return $ Maj Y

-----------------------
-- Entry Point
parseH :: AT.Parser CasedLetter
parseH = (AT.char 'ჰ' $> Min H) <|> (AT.char 'Ⴠ' $> Maj H) <|> (AT.char 'Ჰ' $> Maj H)

---------------------------------------------------------------
-- Parsing Vowels

parseA  :: AT.Parser CasedLetter
parseA  = (AT.char 'ა' $> Min  A) <|> (AT.char 'Ⴀ' $> Maj  A) <|> (AT.char 'Ა' $> Maj  A)

parseAU :: AT.Parser CasedLetter
parseAU = (AT.char 'ჷ' $> Min AU) <|> (AT.char 'Ⴧ' $> Maj AU) <|> (AT.char 'Ჷ' $> Maj AU)

parseE  :: AT.Parser CasedLetter
parseE  = (AT.char 'ე' $> Min  E) <|> (AT.char 'Ⴄ' $> Maj  E) <|> (AT.char 'Ე' $> Maj  E)

parseI  :: AT.Parser CasedLetter
parseI  = (AT.char 'ი' $> Min  I) <|> (AT.char 'Ⴈ' $> Maj  I) <|> (AT.char 'Ი' $> Maj  I)

parseO  :: AT.Parser CasedLetter
parseO  = (AT.char 'ო' $> Min  O) <|> (AT.char 'Ⴍ' $> Maj  O) <|> (AT.char 'Ო' $> Maj  O)

parseU  :: AT.Parser CasedLetter
parseU  = (AT.char 'უ' $> Min  U) <|> (AT.char 'Ⴓ' $> Maj  U) <|> (AT.char 'Უ' $> Maj  U)

---------------------------------------------------------------
-- The Full Parser

parseGeorgianLetter :: AT.Parser CasedLetter
parseGeorgianLetter = AT.choice 
  [parseA,parseE,parseI,parseO,parseU,parseAU
  ,parseY
  ,parseK,parseKY,parseQ,parseQY
  ,parseG,parseGU
  ,parseX,parseXU
  ,parseD,parseDZ,parseDL
  ,parseP,parsePY,parseB
  ,parseT,parseTY,parseTS,parseTSY,parseTL,parseTLY
  ,parseS
  ,parseM,parseN,parseJ,parseL,parseLH,parseW
  ,parseH
  ]

-- Parse non-alphabetical and non-apostrophe characters
-- until next Umista Char.
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not $ isAlpha x)

parseGeorgianChar :: AT.Parser CasedChar
parseGeorgianChar = (Kwak <$> parseGeorgianLetter) <|> (Punct <$> T.singleton <$> AT.anyChar)

parseGeorgianCharNew :: AT.Parser CasedChar
parseGeorgianCharNew = (Kwak <$> parseGeorgianLetter) <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

-- | Parser for the Georgian orthography for Kwak'wala.
-- Use this together with `AT.parseOnly` or similar
-- functions if you want error messages.
--
-- Handles Mkhedruli, Asomtavruli, and Mtavruli,
-- treating Mkhedruli as lower-case, and the
-- other scripts as upper-case.
parseGeorgian :: AT.Parser [CasedChar]
parseGeorgian = AT.many1 parseGeorgianCharNew

-- | Direct encoder for the Georgian othography.
-- Note that if the parser runs into any errors,
-- this just returns an empty list. If you want
-- error messages, use `parseGeorgian`.
--
-- Handles Mkhedruli, Asomtavruli, and Mtavruli,
-- treating Mkhedruli as lower-case, and the
-- other scripts as upper-case.
encodeFromGeorgian :: T.Text -> [CasedChar]
encodeFromGeorgian txt = fromRight [] $ AT.parseOnly parseGeorgian txt


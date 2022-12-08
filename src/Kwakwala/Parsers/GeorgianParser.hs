module Kwakwala.Parsers.GeorgianParser
    ( KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    , encodeFromGeorgian
    , parseGeorgian
    ) where

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

import Data.Either

import System.IO

fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

-- ʷᵂ
isLabial :: Char -> Bool
isLabial 'ვ' = True
isLabial  _  = False

isW = isLabial

---------------------------------------------------------------
-- Parsing K

-----------------------
-- Entry Point
parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ქ' || x == 'Ⴕ')
    ; AT.peekChar >>= parseK' b
    }
-- asdfzxcv

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b KW)
    | otherwise     = return $ makeCase b K
-- asdfzxcv

parseKY :: AT.Parser CasedLetter
parseKY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'კ' || x == 'Ⴉ')
    ; AT.peekChar >>= parseKY' b
    }
-- awsdfzxcv

parseKY' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKY' b Nothing = return $ makeCase b KY
parseKY' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b KWY)
    | otherwise     = return $ makeCase b KY
-- asdfzxcv

---------------------------------------------------------------
-- Parsing Q

-----------------------
-- Entry Point
parseQ :: AT.Parser CasedLetter
parseQ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჴ' || x == 'Ⴤ')
    ; AT.peekChar >>= parseQ' b
    }
-- asdfzxcv

parseQ' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQ' b Nothing = return $ makeCase b Q
parseQ' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b QW)
    | otherwise     = return $ makeCase b Q
-- asdfzxcv

parseQY :: AT.Parser CasedLetter
parseQY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ყ' || x == 'Ⴗ')
    ; AT.peekChar >>= parseQY' b
    }
-- asdfzxcv

parseQY' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY' b Nothing = return $ makeCase b QY
parseQY' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY
-- awsdfzxcv

---------------------------------------------------------------
-- Parsing G

-----------------------
-- Entry Point
parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'გ' || x == 'Ⴂ')
    ; AT.peekChar >>= parseG' b
    }
-- asdfzxcv

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b GW) -- AT.peekChar >>= parseGW b
    | otherwise     = return $ makeCase b G
-- asdfzxcv

-- Ǧǧ
-- Ǧ = \x1e6
-- ǧ = \x1e7

-----------------------
-- Entry Point
parseGU :: AT.Parser CasedLetter
parseGU = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ღ' || x == 'Ⴖ')
    ; AT.peekChar >>= parseGU' b
    }
-- asdfzxc

parseGU' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGU' b Nothing = return $ makeCase b GU
parseGU' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU
-- asfdzxcv

---------------------------------------------------------------
-- Parsing X

-----------------------
-- Entry Point
parseX :: AT.Parser CasedLetter
parseX = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'რ' || x == 'Ⴐ')
    ; AT.peekChar >>= parseX' b
    }
-- asdfzxcv

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing = return $ makeCase b X
parseX' b (Just x)
    | isW         x = AT.anyChar >> (return $ makeCase b XW) -- AT.peekChar >>= parseXW b
    | otherwise     = return $ makeCase b X
-- asdfzxcv

parseXU :: AT.Parser CasedLetter
parseXU = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ხ' || x == 'Ⴞ')
    ; AT.peekChar >>= parseXU' b
    }
-- asdfzxcv

parseXU' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU' b Nothing = return $ makeCase b XU
parseXU' b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU
-- asdfzxcv

---------------------------------------------------------------
-- Parsing D

-----------------------
-- Entry Point
parseD :: AT.Parser CasedLetter
parseD = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'დ' || x == 'Ⴃ')
    ; return $ makeCase b D
--  ; AT.peekChar >>= parseD' b
    }
-- asdfzxcv

-- parseD' :: Bool -> Maybe Char -> AT.Parser CasedLetter
-- parseD' b Nothing = return $ makeCase b D
-- parseD' b (Just x)
--     | (x == 'z' || x == 'Z' || x == 'ᶻ') = AT.anyChar >> (return $ makeCase b DZ)
--     | otherwise                          = return $ makeCase b D
-- asdfzxcv

-----------------------
-- Entry Point
parseDZ :: AT.Parser CasedLetter
parseDZ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ძ' || x == 'Ⴛ')
    ; return $ makeCase b DZ
    }
-- asdfzxcv

---------------------------------------------------------------
-- Parsing B, P, T, C, and S

-----------------------
-- Entry Point
parseB :: AT.Parser CasedLetter
parseB = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ბ' || x == 'Ⴁ')
    ; return $ makeCase b B
    }
-- asdfzxcv

-----------------------
-- Entry Point
parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ფ' || x == 'Ⴔ')
    ; return $ makeCase b P
    }
-- asdfzxcv

parsePY :: AT.Parser CasedLetter
parsePY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'პ' || x == 'Ⴎ')
    ; return $ makeCase b PY
    }
-- asdfzxcv

{-
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
-}
-----------------------
-- Entry Point
parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'თ' || x == 'Ⴇ')
    ; return $ makeCase b T
    }
-- asfdzxcv

parseTY :: AT.Parser CasedLetter
parseTY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ტ' || x == 'Ⴒ')
    ; return $ makeCase b TY
    }
-- asfdzxcv


-- parseT' :: Bool -> Maybe Char -> AT.Parser CasedLetter
-- parseT' b Nothing = return $ makeCase b T
-- parseT' b (Just x)
--     | isApost x = AT.anyChar >> (return $ makeCase b TY)
--     | otherwise = return $ makeCase b T
-- asdfzxcv

-----------------------
-- Entry Point
parseTS :: AT.Parser CasedLetter
parseTS = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ც' || x == 'Ⴚ')
    ; return $ makeCase b TS
    }
-- asfdzxcv

parseTSY :: AT.Parser CasedLetter
parseTSY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'წ' || x == 'Ⴜ')
    ; return $ makeCase b TSY
    }
-- asfdzxcv

-- parseC' :: Bool -> Maybe Char -> AT.Parser CasedLetter
-- parseC' b Nothing = return $ makeCase b TS
-- parseC' b (Just x)
--     | isApost x = AT.anyChar >> (return $ makeCase b TSY)
--     | otherwise = return $ makeCase b TS
-- asdfzxcv

-----------------------
-- Entry Point
parseS :: AT.Parser CasedLetter
parseS = (AT.char 'ს' $> Min S) <|> (AT.char 'Ⴑ' $> Maj S)

---------------------------------------------------------------
-- Parsing M and N

-----------------------
-- Entry Point
parseM :: AT.Parser CasedLetter
parseM = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'მ' || x == 'Ⴋ')
--  ; AT.peekChar >>= parseM' b
    ; return $ makeCase b M
    }
-- asfdzxcv

{-
parseM' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseM' b Nothing = return $ makeCase b M
parseM' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b MY)
    | otherwise = return $ makeCase b M
-- asdfzxcv
-}

-----------------------
-- Entry Point
parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ნ' || x == 'Ⴌ')
--  ; AT.peekChar >>= parseN' b
    ; return $ makeCase b N
    }
-- asfdzxcv

{-
parseN' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseN' b Nothing = return $ makeCase b N
parseN' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b NY)
    | otherwise = return $ makeCase b N
-- asdfzxcv
-}


---------------------------------------------------------------
-- Parsing J/Y, L, LH, and W

-----------------------
-- Entry Point
parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჲ' || x == 'Ⴢ')
--  ; AT.peekChar >>= parseJ' b
    ; return $ makeCase b J
    }
-- asfdzxcv

{-
parseJ' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseJ' b Nothing = return $ makeCase b J
parseJ' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b JY)
    | otherwise = return $ makeCase b J
-- asdfzxcv
-}

-----------------------
-- Entry Point
parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ლ' || x == 'Ⴊ')
--  ; AT.peekChar >>= parseL' b
    ; return $ makeCase b L
    }
-- asfdzxcv

{-
parseL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseL' b Nothing = return $ makeCase b L
parseL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b LY)
    | otherwise = return $ makeCase b L
-- asdfzxcv
-}

-----------------------
-- Entry Point
-- Taken directly from the Umista parser
-- (Then added alternative for upper-case tilde-L)
-- parseLH :: AT.Parser CasedLetter
-- parseLH = ((AT.satisfy (\x -> x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ')) $> Min LH) <|> (AT.char 'Ł' $> Maj LH) <|> (AT.char 'Ɫ' $> Maj LH)
-- Ɫ == U+2C62 == \x2c62

parseLH :: AT.Parser CasedLetter
parseLH = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'შ' || x == 'Ⴘ')
    ; return $ makeCase b LH
    }
-- asfdzxcv


-----------------------
-- Entry Point
parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ვ' || x == 'Ⴅ')
--  ; AT.peekChar >>= parseW' b
    ; return $ makeCase b W
    }
-- asfdzxcv

{-
parseW' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseW' b Nothing = return $ makeCase b W
parseW' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b WY)
    | otherwise = return $ makeCase b W
-- asdfzxcv
-}

---------------------------------------------------------------
-- Parsing λ and ƛ

-----------------------
-- Entry Point
parseDL :: AT.Parser CasedLetter
parseDL = (AT.char 'ჯ' $> Min DL) <|> (AT.char 'Ⴟ' $> Maj DL)

-----------------------
-- Entry Point
parseTL :: AT.Parser CasedLetter
parseTL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჩ' || x == 'Ⴙ')
--  ; AT.peekChar >>= parseTL' b
    ; return $ makeCase b TL
    }
-- asfdzxcv

parseTLY :: AT.Parser CasedLetter
parseTLY = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ჭ' || x == 'Ⴝ')
--  ; AT.peekChar >>= parseTL' b
    ; return $ makeCase b TLY
    }
-- asfdzxcv


{-
parseTL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL' b Nothing = return $ makeCase b TL
parseTL' b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = return $ makeCase b TL
-- asdfzxcv
-}

---------------------------------------------------------------
-- Parsing ʔ and H

-----------------------
-- Entry Point
{-
parseY :: AT.Parser CasedLetter
parseY = do
    { AT.char 'ʔ'
    ; b <- tstm isUpper <$> AT.peekChar
    ; return $ makeCase b Y
    }
    where tstm p Nothing  = False
          tstm p (Just x) = p x
-- asdfzxcv
-}

parseY :: AT.Parser CasedLetter
parseY = AT.char 'ჸ' >> AT.peekChar >>= parseY'

parseY' :: (Maybe Char) -> AT.Parser CasedLetter
parseY' Nothing = return (Min Y)
parseY' (Just x)
    | (x == 'მ'  || x == 'Ⴋ') = AT.anyChar >> (return $ makeCase b MY)
    | (x == 'ნ'  || x == 'Ⴌ') = AT.anyChar >> (return $ makeCase b NY)
    | (x == 'ლ' || x == 'Ⴊ') = AT.anyChar >> (return $ makeCase b LY)
    | (x == 'ჲ'  || x == 'Ⴢ') = AT.anyChar >> (return $ makeCase b JY)
    | (x == 'ვ'  || x == 'Ⴅ') = AT.anyChar >> (return $ makeCase b WY)
    | otherwise              = return $ makeCase b Y -- i.e., the glottal stop takes on the case of the following letter
    where b = isUpper x
-- asdfzxcv

-----------------------
-- Entry Point
parseH :: AT.Parser CasedLetter
parseH = (AT.char 'ჰ' $> Min H) <|> (AT.char 'Ⴠ' $> Maj H)

---------------------------------------------------------------
-- Parsing Vowels

-- Ə == \399 == \x18f
-- ə == \601 == \x259

-- I forgot to substitute the right vowels in... whoops...
parseA  :: AT.Parser CasedLetter
parseA  = (AT.char 'ა' $> Min  A) <|> (AT.char 'Ⴀ' $> Maj  A)

parseAU :: AT.Parser CasedLetter
parseAU = (AT.char 'ჷ' $> Min AU) -- <|> (AT.char 'ჷ' $> Maj AU)

parseE  :: AT.Parser CasedLetter
parseE  = (AT.char 'ე' $> Min  E) <|> (AT.char 'Ⴄ' $> Maj  E)

parseI  :: AT.Parser CasedLetter
parseI  = (AT.char 'ი' $> Min  I) <|> (AT.char 'Ⴈ' $> Maj  I)

parseO  :: AT.Parser CasedLetter
parseO  = (AT.char 'ო' $> Min  O) <|> (AT.char 'Ⴍ' $> Maj  O)

parseU  :: AT.Parser CasedLetter
parseU  = (AT.char 'უ' $> Min  U) <|> (AT.char 'Ⴓ' $> Maj  U)

---------------------------------------------------------------
-- The Full Parser

parseGeorgianLetter :: AT.Parser CasedLetter
parseGeorgianLetter = AT.choice [parseA,parseE,parseI,parseO,parseU,parseAU
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
-- asdfzxcv

-- Parse non-alphabetical and non-apostrophe characters
-- until next Umista Char.
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not $ isAlpha x)

parseGeorgianChar :: AT.Parser CasedChar
parseGeorgianChar = (Kwak <$> parseGeorgianLetter) <|> (Punct <$> T.singleton <$> AT.anyChar)

parseGeorgianCharNew :: AT.Parser CasedChar
parseGeorgianCharNew = (Kwak <$> parseGeorgianLetter) <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

parseGeorgian :: AT.Parser [CasedChar]
parseGeorgian = AT.many1 parseGeorgianCharNew

encodeFromGeorgian :: T.Text -> [CasedChar]
encodeFromGeorgian txt = fromRight [] $ AT.parseOnly parseGeorgian txt





module Kwakwala.Parsers.Boas
    ( KwakLetter(..)
    , CasedLetter(..)
    , CasedChar(..)
    , encodeFromBoas
    , parseBoas
    ) where

import Data.Attoparsec.Text qualified as AT

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T

import Control.Monad
import Control.Applicative

import Data.Functor
import Data.List
import Data.Char

import Kwakwala.Sounds

import Data.Either
import Data.Maybe

-------------------------------------------
-- Helper Functions

-- ʟ

-- Umlaut (or double line)
isUmlaut :: Char -> Bool
isUmlaut '\x308' = True
isUmlaut '\x30e' = True -- double line above
isUmlaut '\x30b' = True -- double accute accent
isUmlaut '\x30f' = True -- double grave accent
isUmlaut _       = False

-- see also:
-- 0x2f5 (modifier letter middle double grave)
-- 0x2f6 (modifier letter middle double acute)

isDotBelow :: Char -> Bool
isDotBelow '\x323' = True
isDotBelow _       = False

isDotAfter :: Char -> Bool
isDotAfter '\x0b7'  = True
isDotAfter '\x2d9'  = True -- dot above
isDotAfter '\x358'  = True -- combining dot above right
isDotAfter '\x18df' = True -- Canadian syllabics raised dot
isDotAfter '\x2022' = True -- bullet
isDotAfter '\x22c5' = True -- dot operator
isDotAfter _        = False

isCircumflex :: Char -> Bool
isCircumflex '\x302' = True
isCircumflex _       = False

isGlottal :: Char -> Bool
isGlottal '\x1d4b' = True -- ᵋ (modifier 'open e' (epsilon))
isGlottal '\x3b5'  = True -- ε (plain epsilon)
isGlottal _        = False

isLabial :: Char -> Bool
isLabial '\x1d58' = True -- ᵘ (modifier u)
isLabial 'w'      = True
isLabial _        = False

isW :: Char -> Bool
isW = isLabial

isEject :: Char -> Bool
isEject = (== '!')

-------------------------------------------
-- Helper Parsers

-- Consume a character if the next character
-- satisfies a predicate.
satisfyMaybe :: (Char -> Bool) -> AT.Parser (Maybe Char)
satisfyMaybe p = (fx <$> AT.peekChar) >>= (maybe (return Nothing) (\x -> AT.anyChar $> Just x))
    where fx Nothing  = Nothing
          fx (Just x) = if (p x) then (Just x) else Nothing

-------------------------------------------
-- Final Parsers

-- Handles start of words,
-- where glottal stops aren't notated
-- (Taken from Umista Parser)
parseBoasWord :: AT.Parser [CasedLetter]
parseBoasWord = parseBoasLetter >>= parseBoasWord'

parseBoasWord' :: CasedLetter -> AT.Parser [CasedLetter]
parseBoasWord' ltr
    | (isKwkVow' ltr) = ([caseOf ltr Y,ltr] ++) <$> many parseBoasLetter
    | otherwise       = (ltr:)                  <$> many parseBoasLetter
    where caseOf (Maj _) = Maj
          caseOf (Min _) = Min

parseBoasLetter :: AT.Parser CasedLetter
parseBoasLetter = AT.choice [parseA,parseE,parseI,parseO,parseU,parseAU
                            ,parseY,parseTL
                            ,parseQ
                            ,parseK,parseG,parseX
                            ,parseP,parseT,parseM,parseN
                            ,parseL,parseW,parseY,parseB,parseH
                            ,parseD,parseLH,parseJ,parseS
                            ,parseZ
                            ]

-- For Parsing 'Escaped' Text
parsePipe :: AT.Parser CasedChar
parsePipe = Punct <$> ((AT.char '|') `comb1` (AT.takeWhile1 (/= '|')) `comb2` (AT.char '|'))
    where comb1 = liftM2 (T.cons)
          comb2 = liftM2 (T.snoc)

-- These next 5 functions are all from
-- the Umista parser
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> (x /= '|' && (not $ isAlpha x))) -- (not . isAlpha)

parseBoasChar :: AT.Parser CasedChar
parseBoasChar = (Kwak <$> parseBoasLetter) <|> parsePipe <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

parseBoasMain :: AT.Parser [CasedChar]
parseBoasMain = (map Kwak <$> parseBoasWord) <|> ((:[]) <$> parsePipe) <|> ((:[]) <$> parsePuncts) <|> ((:[]) <$> Punct <$> T.singleton <$> AT.anyChar)

parseBoas :: AT.Parser [CasedChar]
parseBoas = concat <$> AT.many1 parseBoasMain

encodeFromBoas :: T.Text -> [CasedChar]
encodeFromBoas txt = fixVowels $ fromRight [] $ AT.parseOnly parseBoas txt

-- Adds a glottal stop between vowels
fixVowels :: [CasedChar] -> [CasedChar]
fixVowels []  = []
fixVowels [x] = [x]
fixVowels (x:y:xs)
    | (isKwkVow'' x && isKwkVow'' y) = (x:(Kwak(Min Y)):(fixVowels (y:xs)) )
    | otherwise                      = (x:(fixVowels (y:xs)))

-------------------------------------------
-- Main Parsers

---------------------------
-- Simple Plosives

parseB :: AT.Parser CasedLetter
parseB = (AT.char 'b' $> Min B) <|> (AT.char 'B' $> Maj B)

parseP :: AT.Parser CasedLetter
parseP = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'p' || x == 'P')
    ; z <- satisfyMaybe isEject
    ; maybe (return $ makeCase b P) (const $ return $ makeCase b PY) z
    }

---------------------------
-- Plain Sonorants (and fricatives)

parseL :: AT.Parser CasedLetter
parseL = (AT.char 'l' $> Min L) <|> (AT.char 'L' $> Maj L)

parseW :: AT.Parser CasedLetter
parseW = (AT.char 'w' $> Min W) <|> (AT.char 'W' $> Maj W)

parseM :: AT.Parser CasedLetter
parseM = (AT.char 'm' $> Min M) <|> (AT.char 'M' $> Maj M)

parseN :: AT.Parser CasedLetter
parseN = (AT.char 'n' $> Min N) <|> (AT.char 'N' $> Maj N)

parseJ :: AT.Parser CasedLetter
parseJ = (AT.char 'y' $> Min J) <|> (AT.char 'Y' $> Maj J)

parseS :: AT.Parser CasedLetter
parseS = (AT.char 's' $> Min S) <|> (AT.char 'S' $> Maj S)

parseH :: AT.Parser CasedLetter
parseH = (AT.char 'h' $> Min H) <|> (AT.char 'H' $> Maj H)

-- ł
parseLH :: AT.Parser CasedLetter
parseLH = (AT.char 'ł' $> Min LH) <|> (AT.char 'Ł' $> Maj LH)

---------------------------
-- D etc...

parseD :: AT.Parser CasedLetter
parseD = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'd' || x == 'D')
    ; AT.peekChar >>= parseD' b
    }

parseD' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseD' b Nothing = return $ makeCase b D
parseD' b (Just x)
    | (x == 'z' || x == 'Z') = AT.anyChar $> (makeCase b DZ)
    | otherwise              = return $ makeCase b D

parseZ :: AT.Parser CasedLetter
parseZ = (AT.char 'z' $> Min DZ) <|> (AT.char 'Z' $> Maj DZ)

---------------------------
-- T etc...

parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 't' || x == 'T')
    ; AT.peekChar >>= parseT' b
    }

parseT' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseT' b Nothing = return $ makeCase b T
parseT' b (Just x)
    | (x == 's' || x == 'S') = AT.anyChar >> AT.peekChar >>= parseTS b
    | (isEject x           ) = AT.anyChar >> AT.peekChar >>= parseTY b
    | otherwise              = return $ makeCase b T

parseTS :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseTS b Nothing = return $ makeCase b TS
parseTS b (Just x)
    | (isEject x) = AT.anyChar $> (makeCase b TSY)
    | (otherwise) = return $ makeCase b TS

parseTY :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseTY b Nothing = return $ makeCase b TY
parseTY b (Just x)
    | (x == 's' || x == 'S') = AT.anyChar $> (makeCase b TSY)
    | otherwise              = return $ makeCase b TY

---------------------------
-- K etc...

parseK :: AT.Parser CasedLetter
parseK = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'k' || x == 'K')
    ; AT.peekChar >>= parseK' b
    }

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K -- technically incorrect
parseK' b (Just x)
    | isLabial   x = AT.anyChar >> AT.peekChar >>= parseKW b -- incorrect only
    | isEject    x = AT.anyChar >> AT.peekChar >>= parseKE b
    | isDotAfter x = AT.anyChar >> AT.peekChar >>= parseKD b
    | otherwise    = return $ makeCase b K -- technically incorrect

parseKD :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKD b Nothing = return $ makeCase b K
parseKD b (Just x)
    | isEject x = AT.anyChar $> (makeCase b KY)
    | otherwise = return $ makeCase b K

parseKE :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKE b Nothing = return $ makeCase b KY
parseKE b (Just x)
    | isLabial x = AT.anyChar $> (makeCase b KWY)
    | otherwise  = return $ makeCase b KY

parseKW :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseKW b Nothing = return $ makeCase b KW
parseKW b (Just x)
    | isEject x = AT.anyChar $> (makeCase b KWY)
    | otherwise = return $ makeCase b KW

---------------------------
-- Q etc... (copied from K)

parseQ :: AT.Parser CasedLetter
parseQ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'q' || x == 'Q')
    ; AT.peekChar >>= parseQ' b
    }

parseQ' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQ' b Nothing = return $ makeCase b Q
parseQ' b (Just x)
    | isLabial   x = AT.anyChar >> AT.peekChar >>= parseQW b
    | isEject    x = AT.anyChar >> AT.peekChar >>= parseQE b
    | otherwise    = return $ makeCase b Q

parseQE :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQE b Nothing = return $ makeCase b QY
parseQE b (Just x)
    | isLabial x = AT.anyChar $> (makeCase b QWY)
    | otherwise  = return $ makeCase b QY

parseQW :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseQW b Nothing = return $ makeCase b QW
parseQW b (Just x)
    | isEject x = AT.anyChar $> (makeCase b QWY)
    | otherwise = return $ makeCase b QW

---------------------------
-- G etc... (copied from k)

parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G -- technically incorrect
parseG' b (Just x)
    | isLabial   x = AT.anyChar >> AT.peekChar >>= parseGW b -- incorrect only
    | isDotBelow x = AT.anyChar >> AT.peekChar >>= parseGU b
    | isDotAfter x = AT.anyChar $> (makeCase b G)
    | otherwise    = return $ makeCase b G -- technically incorrect
-- asdfzxcv

parseGU :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGU b Nothing = return $ makeCase b GU
parseGU b (Just x)
    | isLabial x = AT.anyChar $> (makeCase b GUW)
    | otherwise  = return $ makeCase b GU

parseGW :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseGW b Nothing = return $ makeCase b GW
parseGW b (Just x)
    | isDotBelow x = AT.anyChar $> (makeCase b GUW)
    | otherwise    = return $ makeCase b GW

---------------------------
-- X etc...

parseX :: AT.Parser CasedLetter
parseX = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'x' || x == 'X')
    ; AT.peekChar >>= parseX' b
    }

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing = return $ makeCase b XU
parseX' b (Just x)
    | isDotBelow x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isLabial   x = AT.anyChar >> (return $ makeCase b XUW) -- AT.peekChar >>= parseXW b
    | isDotAfter x = AT.anyChar >> (return $ makeCase b X)
    | otherwise     = return $ makeCase b XU

parseXU :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseXU b Nothing = return $　makeCase b X
parseXU b (Just x)
    | isLabial x = AT.anyChar $> (makeCase b XW)
    | otherwise  = return $ makeCase b X

---------------------------
-- TL/DL (smallcaps L)

parseTL :: AT.Parser CasedLetter
parseTL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ʟ' || x == 'Ⱡ' || x == 'Ƚ')
    ; AT.peekChar >>= parseTL' b
    }

parseTL' :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL' b Nothing = return $ makeCase b TL
parseTL' b (Just x)
    | (  x == '!'  ) = AT.anyChar $> (makeCase b TLY)
    | (isDotBelow x) = AT.anyChar $> (makeCase b DL )
    | otherwise      = return $ makeCase b TL

---------------------------
-- Glottal Stop

-- Taken from the Umista Parser
parseY :: AT.Parser CasedLetter
parseY = AT.satisfy isGlottal >> AT.peekChar >>= parseY'

parseY' :: (Maybe Char) -> AT.Parser CasedLetter
parseY' Nothing = return (Min Y)
parseY' (Just x)
    | (x == 'm' || x == 'M') = AT.anyChar >> (return $ makeCase b MY)
    | (x == 'n' || x == 'N') = AT.anyChar >> (return $ makeCase b NY)
    | (x == 'l' || x == 'L') = AT.anyChar >> (return $ makeCase b LY)
    | (x == 'y' || x == 'Y') = AT.anyChar >> (return $ makeCase b JY)
    | (x == 'w' || x == 'W') = AT.anyChar >> (return $ makeCase b WY)
    | otherwise              = return $ makeCase b Y -- i.e., the glottal stop takes on the case of the following letter
    where b = isUpper x

---------------------------
-- Vowels

parseA :: AT.Parser CasedLetter
parseA = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'a' || x == 'A' || x == 'á' || x == 'Á' || x == 'à' || x == 'À')
    ; AT.peekChar >>= parseA' b
    }

parseA' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseA' b Nothing = return $ makeCase b A
parseA' b (Just x)
    | (isUmlaut     x) = AT.anyChar $> (makeCase b E)
    | (isCircumflex x) = AT.anyChar $> (makeCase b O)
    | otherwise        = return $ makeCase b A

parseE  :: AT.Parser CasedLetter
parseE  = (AT.char 'ä' $> Min  E) <|> (AT.char 'Ä' $> Maj  E)

parseI :: AT.Parser CasedLetter
parseI = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'i' || x == 'I')
    ; AT.peekChar >>= parseI' b
    }

parseI' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseI' b Nothing = return $ makeCase b I
parseI' b (Just x)
    | (isCircumflex x) = AT.anyChar $> (makeCase b AU)
    | otherwise        = return $ makeCase b I

parseO  :: AT.Parser CasedLetter
parseO  = (AT.char 'â' $> Min  O) <|> (AT.char 'Â' $> Maj  O)

parseU :: AT.Parser CasedLetter
parseU = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'u' || x == 'U')
    ; AT.peekChar >>= parseU' b
    }

parseU' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseU' b Nothing = return $ makeCase b U
parseU' b (Just x)
    | (isCircumflex x) = AT.anyChar $> (makeCase b AU)
    | otherwise        = return $ makeCase b U

parseAU :: AT.Parser CasedLetter
parseAU = (AT.satisfy (\x -> x == 'ᴇ' || x == 'î' || x == 'û') $> Min AU) <|> (AT.satisfy (\x -> x == 'Î' || x == 'Û') $> Maj AU)



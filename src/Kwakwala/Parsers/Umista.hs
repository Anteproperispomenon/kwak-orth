{-|
Module      : Kwakwala.Parsers.Umista
Description : Parser for the U'mista Orthography for Kwak'wala.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This is the module for parsing the U'mista
orthography. Note that there are two versions
for each function. Try using the "-old" functions
if you're having issues with parsing text.
-}

module Kwakwala.Parsers.Umista
    -- * Parsers
    ( parseUmista
    , parseUmistaOld
    -- * Direct Encoders
    , encodeFromUmista
    , encodeFromUmistaOld
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
import Kwakwala.Parsers.Helpers

import Data.Either

import System.IO

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
isApost '\x313' = True
-- isApost '\x315' = True
isApost '\x2019' = True
isApost '\x2bc' = True
isApost _ = False

-- For checking after an m/n/etc...
isApost' :: Char -> Bool
-- isApost' '\'' = True -- since this would be a glottal stop
isApost' '`'  = True
isApost' '̕'  = True
isApost' '\x313' = True
-- isApost' '\x315' = True
isApost' _ = False

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

-- x̱

parseK' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseK' b Nothing = return $ makeCase b K
parseK' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseQ  b
    | isApost     x = AT.anyChar >> AT.peekChar >>= parseKY b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseKW b
    | otherwise     = return $ makeCase b K

parseKUN :: AT.Parser CasedLetter
parseKUN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ḵ' || x == 'Ḵ' || x == 'q' || x == 'Q')
    ; AT.peekChar >>= parseQ b
    }

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

-- Resultant Parser
parseQ :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQ b Nothing = return $ makeCase b Q
parseQ b (Just x)
    | isApost x = AT.anyChar >> AT.peekChar >>= parseQY b
    | isW     x = AT.anyChar >> AT.peekChar >>= parseQW b
    | otherwise = return $ makeCase b Q

parseQW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQW b Nothing = return $ makeCase b QW
parseQW b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QW

parseQY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseQY b Nothing = return $ makeCase b QW
parseQY b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b QWY)
    | otherwise = return $ makeCase b QY

------------------------------------------------------------------------

parseG :: AT.Parser CasedLetter
parseG = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'g' || x == 'G')
    ; AT.peekChar >>= parseG' b
    }

parseG' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseG' b Nothing = return $ makeCase b G
parseG' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseGU b
    | isW         x = AT.anyChar >> (return $ makeCase b GW)
    | otherwise     = return $ makeCase b G

-- Ǥǥ
parseGUN :: AT.Parser CasedLetter
parseGUN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'ǥ' || x == 'Ǥ')
    ; AT.peekChar >>= parseGU b
    }

-- Resultant Parser
parseGU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseGU b Nothing = return $ makeCase b GU
parseGU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b GUW)
    | otherwise = return $ makeCase b GU

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

parseT :: AT.Parser CasedLetter
parseT = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 't' || x == 'T')
    ; AT.peekChar >>= parseT' b
--  ; case x of
--      { (Just y) -> if isApost y then (AT.anyChar >> return TY) else return T
--      ;       _  -> return T
--      }
    }

-- ŁłƚǱǲǳɫɬ

parseT' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseT' b Nothing = return $ makeCase b T
parseT' b (Just x)
    | (isApost x)        = AT.anyChar >> AT.peekChar >>= parseTY b
    | (x == 's' || x == 'S') = AT.anyChar >> AT.peekChar >>= parseTS b
    | (x == 'l' || x == 'L' || x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ' || x == 'Ł') = AT.anyChar >> AT.peekChar >>= parseTL b
    | otherwise = return $ makeCase b T

parseTS' :: AT.Parser CasedLetter
parseTS' = AT.char 'ʦ' >> AT.peekChar >>= parseTS False

parseTY :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTY b Nothing = return $ makeCase b TY
parseTY b (Just x)
    | (x == 's' || x == 'S') = AT.anyChar >> (return $ makeCase b TSY)
    | (x == 'l' || x == 'L' || x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ' || x == 'Ł') = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = (return $ makeCase b TY)

parseTS :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTS b Nothing = return $ makeCase b TS
parseTS b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TSY)
    | otherwise = return $ makeCase b TS

parseTL :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseTL b Nothing = return $ makeCase b TS
parseTL b (Just x)
    | isApost x = AT.anyChar >> (return $ makeCase b TLY)
    | otherwise = return $ makeCase b TL

--------------------------

parseM :: AT.Parser CasedLetter
parseM = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'm' || x == 'M')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost' y then (AT.anyChar >> (return $ makeCase b MY)) else (return $ makeCase b M)
        ;       _  -> return $ makeCase b M
        }
    }

parseN :: AT.Parser CasedLetter
parseN = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'n' || x == 'N')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost' y then (AT.anyChar >> (return $ makeCase b NY)) else (return $ makeCase b N)
        ;       _  -> return $ makeCase b N
        }
    }

-- ŁłƚǱǲǳɫɬ

-- might want to look at '\313' (NOT \x313), which is an
-- upper-case L with an acute accent.
parseL :: AT.Parser CasedLetter
parseL = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'l' || x == 'L')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost' y then (AT.anyChar >> (return $ makeCase b LY)) else (return $ makeCase b L)
        ;       _  -> return $ makeCase b L
        }
    }

parseJ :: AT.Parser CasedLetter
parseJ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'j' || x == 'J' || x == 'y' || x == 'Y')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost' y then (AT.anyChar >> (return $ makeCase b JY)) else (return $ makeCase b J)
        ;       _  -> return $ makeCase b J
        }
    }

parseW :: AT.Parser CasedLetter
parseW = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'w' || x == 'W')
    ; x <- AT.peekChar
    ; case x of
        { (Just y) -> if isApost' y then (AT.anyChar >> (return $ makeCase b WY)) else (return $ makeCase b W)
        ;       _  -> return $ makeCase b W
        }
    }

-------------------------------------


parseD :: AT.Parser CasedLetter
parseD = do
    { x <- AT.satisfy (\x -> x == 'd' || x == 'D')
    ; AT.peekChar >>= parseD' (isUpper x)
    }

parseDZ :: AT.Parser CasedLetter
parseDZ = do
    { b <- isUpper <$> AT.satisfy (\x -> x == 'z' || x == 'Z' || x == 'ǳ' || x == 'Ǳ' || x == 'ǲ')
    ; return $ makeCase b DZ
    }

-- ŁłƚǱǲǳɫɬ

parseD' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseD' b Nothing = return $ makeCase b T
parseD' b (Just x)
--  | (isApost x)            = AT.anyChar >> AT.peekChar >>= parseTY
    | (x == 'z' || x == 'Z') = AT.anyChar >> (return $ makeCase b DZ)
    | (x == 'l' || x == 'L' || x == 'ł' || x == 'ƚ' || x == 'ɫ' || x == 'ɬ' || x == 'Ł') = AT.anyChar >> (return $ makeCase b DL)
    | otherwise = return $ makeCase b D

-- maybe deal with Ǳǲǳ later

-----------------------------------------------------------

parseX :: AT.Parser CasedLetter
parseX = do
    { x <- AT.satisfy (\x -> x == 'x' || x == 'X')
    ; AT.peekChar >>= parseX' (isUpper x)
    }

parseX' :: Bool -> (Maybe Char) -> AT.Parser CasedLetter
parseX' b Nothing  = return $ makeCase b X
parseX' b (Just x)
    | isUnderline x = AT.anyChar >> AT.peekChar >>= parseXU b
    | isW         x = AT.anyChar >> AT.peekChar >>= parseXW b
    | otherwise     = return $ makeCase b X

parseXW :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXW b Nothing = return $ makeCase b XW
parseXW b (Just x)
    | isUnderline x = AT.peekChar >> (return $ makeCase b XUW)
    | otherwise     = return $ makeCase b XW

-- Resultant Parser
parseXU :: Bool -> Maybe Char -> AT.Parser CasedLetter
parseXU b Nothing = return $ makeCase b XU
parseXU b (Just x)
    | isW     x = AT.anyChar >> (return $ makeCase b XUW)
    | otherwise = return $ makeCase b XU

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
parseY = AT.satisfy isApost >> AT.peekChar >>= parseY'

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
-- a̱
-- x̱

-- 'a̱' == '\x0101'
-- 'Ā' == '\x0100'
parseAU :: AT.Parser CasedLetter
parseAU = (AT.char '\x0101' $> Min AU) <|> (AT.char '\x0100' $> Maj AU)

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

-- Handles start of words,
-- where glottal stops aren't notated
parseUmistaWord :: AT.Parser [CasedLetter]
parseUmistaWord = parseUmistaLetter >>= parseUmistaWord'

parseUmistaWord' :: CasedLetter -> AT.Parser [CasedLetter]
parseUmistaWord' ltr
    | (isKwkVow' ltr) = ([caseOf ltr Y,ltr] ++) <$> many parseUmistaLetter
    | otherwise       = (ltr:)                  <$> many parseUmistaLetter
    where caseOf (Maj _) = Maj
          caseOf (Min _) = Min

parseUmistaWordA :: AT.Parser CasedWord
parseUmistaWordA = parseUmistaLetter >>= parseUmistaWordA'

parseUmistaWordA' :: CasedLetter -> AT.Parser CasedWord
parseUmistaWordA' ltr
    | (isKwkVow' ltr) = KwakW <$> ([caseOf ltr Y,ltr] ++) <$> many parseUmistaLetter
    | otherwise       = KwakW <$> (ltr:)                  <$> many parseUmistaLetter
    where caseOf (Maj _) = Maj
          caseOf (Min _) = Min

parseUmistaLetter :: AT.Parser CasedLetter
parseUmistaLetter = AT.choice [parseA,parseAU,parseE,parseI,parseO,parseU
--                            ,parseSpace
                              ,parseK,parseG,parseKUN,parseGUN,parseX
                              ,parseP,parseT,parseM,parseN
                              ,parseL,parseW,parseY,parseB,parseH
                              ,parseD,parseLH,parseJ,parseS
                              ,parseDZ
                              ,parseTS'
                              ]

-- Parse non-alphabetical and non-apostrophe characters
-- until next Umista Char.
parsePuncts :: AT.Parser CasedChar
parsePuncts = Punct <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x || (x == '|')))

parsePunctsA :: AT.Parser CasedWord
parsePunctsA = PunctW <$> AT.takeWhile1 (\x -> not (isAlpha x || isApost x))

parseUmistaChar :: AT.Parser CasedChar
parseUmistaChar = (Kwak <$> parseUmistaLetter) <|> parsePipe <|> (Punct <$> T.singleton <$> AT.anyChar)

parseUmistaCharNew :: AT.Parser CasedChar
parseUmistaCharNew = (Kwak <$> parseUmistaLetter) <|> parsePipe <|> parsePuncts <|> (Punct <$> T.singleton <$> AT.anyChar)

parseUmistaMain :: AT.Parser [CasedChar]
parseUmistaMain = (map Kwak <$> parseUmistaWord) <|> ((:[]) <$> parsePipe) <|> ((:[]) <$> parsePuncts) <|> ((:[]) <$> Punct <$> T.singleton <$> AT.anyChar)

parseUmistaWords :: AT.Parser [CasedWord]
parseUmistaWords = AT.many1 (parseUmistaWordA <|> parsePunctsA <|> (PunctW <$> T.singleton <$> AT.anyChar))

-- | The main parser for U'mista. Non-U'mista characters
-- are parsed with `AT.takeWhile1`, so this version is
-- more efficient.
parseUmista :: AT.Parser [CasedChar]
parseUmista = concat <$> AT.many1 parseUmistaMain
-- parseUmista = AT.many1 parseUmistaCharNew

-- | Directly convert some U'mista text to a
-- list of `CasedChar`. Note that if the
-- parser fails, this just returns an empty
-- list. If you want actual error handling,
-- use `parseUmista` together with `AT.parseOnly`.
encodeFromUmista :: T.Text -> [CasedChar]
encodeFromUmista txt = fromRight [] $ AT.parseOnly parseUmista txt

-- | An alternate parser for U'mista. Non-U'mista
-- characters are parsed one at a time. Use this
-- if `parseUmista` is having issues.
parseUmistaOld :: AT.Parser [CasedChar]
parseUmistaOld = AT.many1 parseUmistaChar

-- | Directly convert some U'mista text to a
-- list of `CasedChar`. Like `parseUmistaOld`,
-- this parses non-U'mista characters one
-- at a time. Note that if the parser fails,
-- this just returns an empty list. If you
-- want actual error handling, use `parseUmista`
-- together with `AT.parseOnly`.
encodeFromUmistaOld :: T.Text -> [CasedChar]
encodeFromUmistaOld txt = fromRight [] $ AT.parseOnly parseUmistaOld txt
-- Əə



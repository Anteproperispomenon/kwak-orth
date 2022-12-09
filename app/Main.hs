{-# LANGUAGE OverloadedStrings #-}

-- The Main file, that imports all the other stuff

module Main where

-----------------------------------------------
-- Imports

import Data.Attoparsec.Text qualified as AT

import Data.Text          qualified as T
import Data.Text.IO       qualified as T
import Data.Text.Encoding qualified as T

import Data.Text.Lazy qualified as TL

import Control.Monad
import Control.Applicative

import Data.Functor
import Data.List
import Data.Char

import Data.Monoid

import Data.Either

import System.IO
import System.Exit
import System.FilePath
import System.Directory

-- For Parsing Options
import Control.Applicative
import Options.Applicative

import qualified Options.Applicative.Help.Pretty as D

-------------------------------------
-- Other Modules

import qualified TextUTF8 as TU

import Kwakwala.Parsers.Umista
import Kwakwala.Output.UmistaOutputNew
import Kwakwala.Parsers.NapaParser
import Kwakwala.Output.NapaOutput
import Kwakwala.Parsers.Boas

import Kwakwala.Output.PseudoBoasOutput
import Kwakwala.Output.Georgian
import Kwakwala.Parsers.GeorgianParser

import Kwakwala.Output.GrubbAscii
import Kwakwala.Parsers.GrubbAsciiParser

import Kwakwala.Output.IPAOutput

-- fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

data OrthType = Umista | Napa | Umista2 | Boas | PseudoBoas | Georgian | Georgian2 | GrubbAscii | IPA | IPA2 deriving (Show, Eq)

data ArgInput
   = FullInput { inputFile  :: FilePath
               , outputFile :: FilePath
               , inputType  :: OrthType
               , outputType :: OrthType
               , overwrite  :: Bool
               , oldStyle   :: Bool
               , verFlag    :: Bool
               } deriving (Show,Eq)
-- asdfzxcv

data ArgInput'
   = FullInput' { inputFile'  :: FilePath
                , outputFile' :: Maybe FilePath
                , inputType'  :: OrthType
                , outputType' :: OrthType
                , overwrite'  :: Bool
                , oldStyle'   :: Bool
                , verFlag'    :: Bool
                } deriving (Show,Eq)
-- asdfzxcv

-- handleStuff1 (FullInput "sample_text4.txt" "sample_text4_out.txt" Napa Napa False False False)

fileInput :: FilePath -> ArgInput
fileInput inf = FullInput inf outf Umista Napa False False False
    where (fl,xt) = splitExtension inf
          outf    = fl ++ ".napa" ++ xt
-- asdfzxcv

fixInput :: ArgInput' -> ArgInput
fixInput (FullInput' inf (Just outf) inp outp ovr sty ver) = (FullInput inf outf inp outp ovr sty ver)
fixInput (FullInput' inf Nothing     inp outp ovr sty ver) 
    | (outp == Napa      ) = (FullInput inf (outf ".napa") inp outp ovr sty ver)
    | (outp == Umista    ) = (FullInput inf (outf ".umst") inp outp ovr sty ver)
    | (outp == Umista2   ) = (FullInput inf (outf ".ums2") inp outp ovr sty ver)
    | (outp == Boas      ) = (FullInput inf (outf ".boas") inp outp ovr sty ver)
    | (outp == PseudoBoas) = (FullInput inf (outf ".pbos") inp outp ovr sty ver)
    | (outp == Georgian  ) = (FullInput inf (outf ".geor") inp outp ovr sty ver)
    | (outp == Georgian2 ) = (FullInput inf (outf ".geo2") inp outp ovr sty ver)
    | (outp == GrubbAscii) = (FullInput inf (outf ".grba") inp outp ovr sty ver)
    | (outp == IPA       ) = (FullInput inf (outf ".ipa" ) inp outp ovr sty ver)
    | (outp == IPA2      ) = (FullInput inf (outf ".ipa2") inp outp ovr sty ver)
    where (fl,xt) = splitExtension inf
          outf x  = fl ++ x ++ xt
-- asdfzxcv

fullInput :: Parser ArgInput
fullInput = fixInput <$> fullInput'

fullInput' :: Parser ArgInput'
fullInput'
    = FullInput'
        <$> strOption
            ( long  "input"
           <> long  "infile"
           <> short 'i'
           <> metavar "INPUT"
           <> help "Input text file"
            )
        <*> (optional . strOption) -- strOptionMaybe -- i.e. optional . strOption
            ( long "output"
           <> long "outfile"
           <> short 'o'
           <> metavar "OUTPUT"
           <> help "Output text file"
            )
        <*> option parseOrth'
            ( long  "from"
           <> long  "decode"
           <> short 'f'
           <> short 'd'
           <> metavar "ENCODING"
           <> value Umista
           <> help (  "The input file's orthography: "
                   <> "[U]mista, "         -- "\t[U]    : Umista\n"
                   <> "[N]APA, "           -- "\t[N]    : NAPA\n"
                   <> "[B]oas, "           -- "\t[B/PB] : (Pseudo-)Boas\n"
                   <> "[G]rubb-Ascii, "    -- "\t[G]    : Grubb-Ascii\n"
                   <> "or [GR] (Georgian)" -- "\t[GR]   : Georgian\n"
                   )
            )
        <*> option parseOrth'
            ( long  "to"
           <> long  "encode"
           <> short 't'
           <> short 'e'
           <> metavar "ENCODING"
           <> value Napa
           <> help (  "The output file's orthography: "
                   <> "[U]mista, "         -- "\t[U]    : Umista\n"
                   <> "[N]APA, "           -- "\t[N]    : NAPA\n"
                   <> "[B]oas, "           -- "\t[B/PB] : (Pseudo-)Boas\n"
                   <> "[G]rubb-Ascii, "    -- "\t[G]    : Grubb-Ascii\n"
                   <> "[I]PA,  "           -- "\t[I]    : IPA \n"
                   <> "or [GR] (Georgian)" -- "\t[GR]   : Georgian\n"
                   )
            )
        <*> switch
            ( long "overwrite"
           <> short 'w'
           <> help "Overwrite the output file, if it already exists."
            )
        <*> switch
            ( long "oldparse"
           <> short 'x'
           <> help "Use the old-style parser (Parse non-Kwak'wala chars one at a time).\nTry using this if the new parser doesn't work."
            )
        <*> switch
            ( long "version"
           <> short 'v'
           <> help "Display version info of this release and all previous releases."
            )
-- asdfzxcv

-- strOptionMaybe = optional . strOption
-- strOptionMaybe :: Mod OptionFields String -> Parser (Maybe String)
-- strOptionMaybe fld =  f <$> strOption fld
--     where f ""  = Nothing
--           f str = Just str
-- asdfzcxv

parseOrth' :: ReadM OrthType
parseOrth' = eitherReader (AT.parseOnly parseOrth . T.pack)

parseOrth :: AT.Parser OrthType
parseOrth = parseOrthNapa <|> parseOrthUmista <|> parseOrthPseudoBoas <|> parseOrthBoas <|> parseOrthGeorgian <|> parseOrthGrubb <|> parseOrthIPA

parseOrthNapa :: AT.Parser OrthType
parseOrthNapa = (AT.choice ["n","N","NAPA","napa","Napa"]) $> Napa

parseOrthUmista :: AT.Parser OrthType
parseOrthUmista = do 
    { (AT.choice ["u","U","Umista","U'mista","umista","u'mista","UMISTA","U'MISTA","U\"MISTA"])
    ; x <- AT.peekChar
    ; if (x == (Just '2')) then (AT.anyChar $> Umista2) else (return Umista)
    }
-- asdfzxcv

parseOrthPseudoBoas :: AT.Parser OrthType
parseOrthPseudoBoas = (AT.choice ["pb","PB","PBoas","pboas","pseudoboas","pseudo-boas","Pseudo-Boas"]) $> PseudoBoas

parseOrthBoas :: AT.Parser OrthType
parseOrthBoas = (AT.choice ["b","B","Boas","Boas", "BOAS"]) $> Boas

parseOrthIPA :: AT.Parser OrthType
parseOrthIPA = do
    { (AT.choice ["i","I","IPA","ipa"]) $> IPA
    ; x <- AT.peekChar
    ; if (x == (Just '2')) then (AT.anyChar $> IPA2) else (return IPA)
    }
-- asdfzxcv

parseOrthGeorgian :: AT.Parser OrthType
parseOrthGeorgian = do 
    { (AT.choice ["gr","GR","Georgian","georgian","george","George","GEORGIAN","GEORGE"])
    ; x <- AT.peekChar
    ; if (x == (Just '2')) then (AT.anyChar $> Georgian2) else (return Georgian)
    }
-- asdfzxcv

parseOrthGrubb :: AT.Parser OrthType
parseOrthGrubb = (AT.choice ["g","G","Grubb","grubb", "GRUBB", "Grubb-Ascii", "grubb-ascii"]) $> GrubbAscii

sentence1 = "ga̱lsga̱lʦisux̱ da ḵwaḵ̕wanix̱" :: T.Text

mainParser :: Parser ArgInput
mainParser = fullInput -- <|> dragInput

-- Taken from https://github.com/pcapriotti/optparse-applicative#running-parsers
opts :: ParserInfo ArgInput
opts = info (mainParser <**> helper)
    (  fullDesc
    <> progDesc "Convert text files between various Kwak'wala orthographies."
    <> header   "A parser for converting between various Kwak'wala orthographies."
    )

main :: IO ()
main = do
    { TU.fixLocale
    ; options@(FullInput inf outf inp outp w sty ver) <- execParser opts
    ; mapM showVersionInfo (if ver then (versionInfo) else (take versionDisplay versionInfo))
    ; when (inputType  options == PseudoBoas) $ putStrLn "Note: Pseudo-Boas input is the same as Boas input."
    ; when (inputType  options ==       Boas) $ putStrLn "Note: Boas input is still in testing."
--  ; when (outputType options == PseudoBoas) $ putStrLn "Warning: Pseudo-Boas output does not support capitalisation."
    ; when (outputType options ==       Boas) $ putStrLn "Warning: Boas output is the same as Pseudo-Boas output." -- , and does not support capitalisation."
--  ; when (inputType  options ==       Napa) $ putStrLn "Warning: Napa input is still in testing (but I did double-check the code)."
--  ; when (inputType  options == GrubbAscii) $ putStrLn "Warning: Grubb-Ascii is still in very eary stages."
    ; when (inputType  options ==        IPA) $ putStrLn "Warning: IPA input not supported; defaulting to Grubb."
    ; when (inputType  options ==       IPA2) $ putStrLn "Warning: IPA input not supported; defaulting to Grubb."
    ; b1 <- doesFileExist inf
    ; b2 <- doesFileExist outf
    ; unless b1 $ die $ "Could not find file: \"" ++ outf ++ "\".\n"
    ; when (b2 && (not w)) $ die $ "File already exists: \"" ++ outf ++ "\".\n"
    ; when (b2 && w) $ putStrLn $ "Warning: overwriting file: \"" ++ outf ++ "\"."
--  ; hinp <- openInput  options
--  ; hout <- openOutput options
    ; otxt <- withFile inf ReadMode (handleStuff1 options)
    ; withFile outf WriteMode (`TU.hPutStr` otxt)
    ; putStrLn "Successfully converted/fixed file [in theory]."
    }
-- asdfzxcv

----------------------------------
-- Version Info

-- The number of versions to display
versionDisplay :: Int
versionDisplay = 7

versionInfo :: [(String,[String])]
versionInfo 
    = [( "2022-12-08 Onwards"
       , [ "Moving to a proper package/repository."
         , "Cleaning up/formalising code/usage."
         ]
       )
      ,( "2019-09-20-1316"
       , ["Added IPA Output."
         ]
       )
      ,( "2019-06-12-1706"
       , ["Changed Umista Output for DL."
         ]
       )
      ,( "2018-12-26-1430"
       , ["Updated some program descriptions."
         ]
       )
      ,( "2018-11-28-1343"
       , ["Fixed Grubb output for some upper-case letters."
         ]
       )
      ,( "2018-11-28-1337"
       , ["Fixed Grubb output for ts (was outputting c)."
         ]
       )
      ,( "2018-11-28-1313"
       , ["Pipes now work properly."
         ]
       )
      ,( "2018-11-28-1244"
       , ["Added support for escaping characters (Use pipes)."
         ]
       )
      ,( "2018-11-27-1620"
       , ["Grubb output should now ignore glottal marks in words that begin with vowels."
         ]
       )
      ,( "2018-11-27-1610"
       , ["Added preliminary Grubb-Ascii input support."
         ]
       )
      ,( "2018-11-19-1323"
       ,["Added Grubb-Ascii output support (preliminary)."
        ]
       )
      ,( "2018-06-21-1444"
       ,["Fixed Georgian parser so it actually parses \"N\" now."
        ]
       )
      ,( "2018-06-21-1436"
       ,["Added default output filename for Georgian text."
        ,"So now you can actually convert things to Georgian with the batch files."
        ]
       )
      ,( "2018-06-21-1430"
       ,["Added preliminary support for Georgian input/output."
        ,"It's still in testing, so try converting back and forth with it."
        ]
       )
      ,( "2018-06-07-1755"
       ,["Added message on successful exit."
        ]
       )
      ,( "2018-06-07-1737"
       ,["Messed around with this version stuff formatting a little."
        ,"Modified the batch files a little."
        ]
       )
      ,( "2018-06-07-1732"
       ,["Added capitalisation for Pseudo-Boas output."
        ,"Modified the Boas parser slightly to accept new \"uppercase\" character substitutes."
        ,"Removed the option to simply drag and drop onto the executable directly (use the batch files instead)."
        ,"Added these version info thingies (that's why this one is the oldest)."
        ,"Removed some obsolete functions from Main.hs."
        ]
       )
      ]
-- asdfzxcv

showVersionInfo :: (String,[String]) -> IO ()
showVersionInfo (vr,xs) = do
    { putStrLn $ "Version " ++ vr
    ; putStrLn $ replicate (8 + (length vr)) '-'
    ; putStrLn $ "Changes:"
    ; mapM_ (\str -> putStrLn $ "-- " ++ str) xs
    ; putStrLn ""
    }
-- asdfxzcv

-- Note:
--    (`f` x)
-- == (flip f $ x)
-- == (\y -> f y x)

-- Gets the contents of a file
-- and encodes it.
handleStuff1 :: ArgInput -> Handle -> IO T.Text
-- handleStuff1 (FullInput _ _ Napa _ _) _      = die "Napa input is not yet supported."
-- handleStuff1 (FullInput _ _       Boas _ _ _) _ = die        "Boas input is not yet supported."
-- handleStuff1 (FullInput _ _ PseudoBoas _ _ _) _ = die "Pseudo-Boas input is not yet supported."
-- handleStuff1 (FullInput _ _ GrubbAscii _ _ _ _) _ = die "Grubb-Ascii input is not yet supported."
handleStuff1 (FullInput _ _ enc dec _ sty _) hinp = do
    { itxt <- TU.hGetContents hinp
    ; return $ decodeTo dec $ encodeFrom enc sty $ itxt
    }

encodeFrom :: OrthType -> Bool -> T.Text -> [CasedChar]
encodeFrom Umista     = encodeFromUmistaType
encodeFrom Umista2    = encodeFromUmistaType
encodeFrom Napa       = encodeFromNapaType
encodeFrom Boas       = \_ -> encodeFromBoas
encodeFrom PseudoBoas = \_ -> encodeFromBoas
encodeFrom Georgian   = \_ -> encodeFromGeorgian
encodeFrom Georgian2  = \_ -> encodeFromGeorgian
encodeFrom GrubbAscii = \_ -> encodeFromGrubbAscii
encodeFrom IPA        = \_ -> encodeFromGrubbAscii
encodeFrom IPA2       = \_ -> encodeFromGrubbAscii

encodeFromUmistaType :: Bool -> T.Text -> [CasedChar]
encodeFromUmistaType False = encodeFromUmista
encodeFromUmistaType True  = encodeFromUmistaOld

encodeFromNapaType :: Bool -> T.Text -> [CasedChar]
encodeFromNapaType False = encodeFromNapa
encodeFromNapaType True  = encodeFromNapaOld

decodeTo :: OrthType -> [CasedChar] -> T.Text
decodeTo Umista     = decodeToUmista
decodeTo Umista2    = decodeToUmistaAlt
decodeTo Napa       = decodeToNapa
decodeTo Boas       = decodeToPseudoBoas
decodeTo PseudoBoas = decodeToPseudoBoas
decodeTo Georgian   = decodeToGeorgianAlt
decodeTo Georgian2  = decodeToGeorgian
decodeTo GrubbAscii = decodeToGrubbAscii
decodeTo IPA        = decodeToIpa
decodeTo IPA2       = decodeToIpaAlt


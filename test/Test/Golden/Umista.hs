module Test.Golden.Umista 
  ( fixUmistaTests
  , fixUmistaTest
  , fixUmistaViaGrubbTest
  , fixUmistaViaNapaTest
  , fixUmistaViaBoasTest
  , fixUmistaViaGeorgianTest
  , umista2NapaTest
  ) where

-- Testing Input to/from U'mista.

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden
import Test.Golden.Helpers

import Data.ByteString.Lazy qualified as BL

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import Data.Text.Lazy          qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU

fixUmistaTests :: TestName -> String -> String -> TestTree
fixUmistaTests tstNam inFile outExt = testGroup ("U'mista Tests: " ++ tstNam)
  [ fixUmistaTest inFile outExt
  , fixUmistaViaGrubbTest outExt
  , fixUmistaViaNapaTest outExt
  , fixUmistaViaBoasTest outExt
  , fixUmistaViaGeorgianTest outExt
  ]

-- "examples/sample1_umista_raw.txt"

fixUmistaTest :: String -> String -> TestTree
fixUmistaTest inFile outExt = 
  goldenVsString
    "Fix U'mista"
    ("golden/fixedUmista" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt = decodeToUmista $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixUmistaViaGrubbTest :: String -> TestTree
fixUmistaViaGrubbTest outExt = 
  goldenVsString
    "U'mista -> Grubb -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGrubbAscii $ encodeFromUmista     inp
       ; let txt2 = decodeToUmista     $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedUmista" ++ "_" ++ outExt ++ ".golden"

fixUmistaViaNapaTest :: String -> TestTree
fixUmistaViaNapaTest outExt = 
  goldenVsString
    "U'mista -> NAPA  -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToNapa   $ encodeFromUmista inp
       ; let txt2 = decodeToUmista $ encodeFromNapa   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedUmista" ++ "_" ++ outExt ++ ".golden"

fixUmistaViaBoasTest :: String -> TestTree
fixUmistaViaBoasTest outExt = 
  goldenVsString
    "U'mista -> Boas  -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromUmista inp
       ; let txt2 = decodeToUmista     $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedUmista" ++ "_" ++ outExt ++ ".golden"

fixUmistaViaGeorgianTest :: String -> TestTree
fixUmistaViaGeorgianTest outExt = 
  goldenVsString
    "U'mista -> Georgian -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromUmista   inp
       ; let txt2 = decodeToUmista        $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedUmista" ++ "_" ++ outExt ++ ".golden"

umista2NapaTest :: TestTree
umista2NapaTest = 
  goldenVsString
    "U'mista to NAPA"
    "golden/Umista2Napa.golden"
    do { inp <- TU.readFile "examples/sample1_umista.txt"
       ; let txt = decodeToNapa $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

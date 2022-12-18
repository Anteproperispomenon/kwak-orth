module Test.Golden.Umista 
  ( fixUmistaTest
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



fixUmistaTest :: TestTree
fixUmistaTest = 
  goldenVsString
    "Fix U'mista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt = decodeToUmista $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixUmistaViaGrubbTest :: TestTree
fixUmistaViaGrubbTest = 
  goldenVsStringDiff'
    "U'mista -> Grubb -> U'mista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt1 = decodeToGrubbAscii $ encodeFromUmista     inp
       ; let txt2 = decodeToUmista     $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixUmistaViaNapaTest :: TestTree
fixUmistaViaNapaTest = 
  goldenVsStringDiff'
    "U'mista -> NAPA  -> U'mista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt1 = decodeToNapa   $ encodeFromUmista inp
       ; let txt2 = decodeToUmista $ encodeFromNapa   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixUmistaViaBoasTest :: TestTree
fixUmistaViaBoasTest = 
  goldenVsStringDiff'
    "U'mista -> Boas  -> U'mista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt1 = decodeToPseudoBoas $ encodeFromUmista inp
       ; let txt2 = decodeToUmista     $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixUmistaViaGeorgianTest :: TestTree
fixUmistaViaGeorgianTest = 
  goldenVsStringDiff'
    "U'mista -> Georgian -> U'mista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromUmista   inp
       ; let txt2 = decodeToUmista        $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }


umista2NapaTest :: TestTree
umista2NapaTest = 
  goldenVsString
    "U'mista to NAPA"
    "golden/Umista2Napa.golden"
    do { inp <- TU.readFile "examples/sample1_umista.txt"
       ; let txt = decodeToNapa $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

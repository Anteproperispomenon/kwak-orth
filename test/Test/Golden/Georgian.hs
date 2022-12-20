module Test.Golden.Georgian
  ( fixGeorgianTest
  , fixGeorgianViaGrubbTest
  , fixGeorgianViaUmistaTest
  , fixGeorgianViaBoasTest
  , fixGeorgianViaNapaTest
  ) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden
import Test.Golden.Helpers

import Data.ByteString.Lazy qualified as BL

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU

fixGeorgianTest :: TestTree
fixGeorgianTest = 
  goldenVsString
    "Fix Georgian"
    "golden/fixedGeorgian.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt = decodeToGeorgianTitle $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixGeorgianViaGrubbTest :: TestTree
fixGeorgianViaGrubbTest = 
  goldenVsStringDiff'
    "Georgian -> Grubb   -> Georgian"
    "golden/fixedGeorgian.golden"
    do { inp <- TU.readFile "golden/fixedGeorgian.golden"
       ; let txt1 = decodeToGrubbAscii    $ encodeFromGeorgian   inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGeorgianViaUmistaTest :: TestTree
fixGeorgianViaUmistaTest = 
  goldenVsStringDiff'
    "Georgian -> U'mista -> Georgian"
    "golden/fixedGeorgian.golden"
    do { inp <- TU.readFile "golden/fixedGeorgian.golden"
       ; let txt1 = decodeToUmista        $ encodeFromGeorgian   inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromUmista     txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGeorgianViaBoasTest :: TestTree
fixGeorgianViaBoasTest = 
  goldenVsStringDiff'
    "Georgian -> Boas    -> Georgian"
    "golden/fixedGeorgian.golden"
    do { inp <- TU.readFile "golden/fixedGeorgian.golden"
       ; let txt1 = decodeToPseudoBoas $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGeorgianViaNapaTest :: TestTree
fixGeorgianViaNapaTest = 
  goldenVsStringDiff'
    "Georgian -> NAPA    -> Georgian"
    "golden/fixedGeorgian.golden"
    do { inp <- TU.readFile "golden/fixedGeorgian.golden"
       ; let txt1 = decodeToNapa $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromNapa txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }


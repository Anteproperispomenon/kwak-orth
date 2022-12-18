module Test.Golden.Napa
  ( fixNapaTest
  , fixNapaViaGrubbTest
  , fixNapaViaUmistaTest
  , fixNapaViaBoasTest
  , fixNapaViaGeorgianTest
  -- , umista2NapaTest
  ) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden
import Test.Golden.Helpers

import Data.ByteString.Lazy qualified as BL

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

-- import Data.Text.Lazy          qualified as TL
-- import Data.Text.Lazy.Encoding qualified as TL

import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU

fixNapaTest :: TestTree
fixNapaTest = 
  goldenVsString
    "Fix NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt = decodeToNapa $ encodeFromNapa inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixNapaViaGrubbTest :: TestTree
fixNapaViaGrubbTest = 
  goldenVsStringDiff'
    "NAPA -> Grubb    -> NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt1 = decodeToGrubbAscii $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa       $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixNapaViaUmistaTest :: TestTree
fixNapaViaUmistaTest = 
  goldenVsStringDiff'
    "NAPA -> U'mista  -> NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt1 = decodeToUmista $ encodeFromNapa   inp
       ; let txt2 = decodeToNapa   $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixNapaViaBoasTest :: TestTree
fixNapaViaBoasTest = 
  goldenVsStringDiff'
    "NAPA -> Boas     -> NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt1 = decodeToPseudoBoas $ encodeFromNapa inp
       ; let txt2 = decodeToNapa       $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixNapaViaGeorgianTest :: TestTree
fixNapaViaGeorgianTest = 
  goldenVsStringDiff'
    "NAPA -> Georgian -> NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa          $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }








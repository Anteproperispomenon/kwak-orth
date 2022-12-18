module Test.Golden.Grubb
  ( fixGrubbTest
  , fixGrubbViaNapaTest
  , fixGrubbViaUmistaTest
  , fixGrubbViaBoasTest
  , fixGrubbViaGeorgianTest
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

fixGrubbTest :: TestTree
fixGrubbTest = 
  goldenVsString
    "Fix Grubb"
    "golden/fixedGrubb.golden"
    do { inp <- TU.readFile "examples/sample1_grubb.txt"
       ; let txt = decodeToGrubbAscii $ encodeFromGrubbAscii inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixGrubbViaNapaTest :: TestTree
fixGrubbViaNapaTest = 
  goldenVsStringDiff'
    "Grubb -> NAPA     -> Grubb"
    "golden/fixedGrubb.golden"
    do { inp <- TU.readFile "examples/sample1_grubb.txt"
       ; let txt1 = decodeToNapa       $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromNapa       txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaUmistaTest :: TestTree
fixGrubbViaUmistaTest = 
  goldenVsStringDiff'
    "Grubb -> U'mista  -> Grubb"
    "golden/fixedGrubb.golden"
    do { inp <- TU.readFile "examples/sample1_grubb.txt"
       ; let txt1 = decodeToUmista $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaBoasTest :: TestTree
fixGrubbViaBoasTest = 
  goldenVsStringDiff'
    "Grubb -> Boas     -> Grubb"
    "golden/fixedGrubb.golden"
    do { inp <- TU.readFile "examples/sample1_grubb.txt"
       ; let txt1 = decodeToPseudoBoas $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromBoas       txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaGeorgianTest :: TestTree
fixGrubbViaGeorgianTest = 
  goldenVsStringDiff'
    "Grubb -> Georgian -> Grubb"
    "golden/fixedGrubb.golden"
    do { inp <- TU.readFile "examples/sample1_grubb.txt"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii    $ encodeFromGeorgian   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

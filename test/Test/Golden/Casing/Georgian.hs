module Test.Golden.Casing.Georgian
  ( georgianAllLower
  , georgianAllUpper
  , georgianCaseCompare
  , checkGeorgianViaGrubb
  , checkGeorgianViaBoas
  , checkGeorgianViaUmista
  ) where

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden
import Test.Golden.Helpers
import Test.Helpers

import Data.ByteString.Lazy qualified as BL

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import Kwakwala.Sounds
import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU

georgianAllLower :: TestTree
georgianAllLower = 
  goldenVsString
    "Make Georgian Lowercase"
    "golden/georgianLower.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGeorgianTitle chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

georgianAllUpper :: TestTree
georgianAllUpper = 
  goldenVsString
    "Make Georgian Uppercase"
    "golden/georgianUpper.golden"
    do { inp <- TU.readFile "golden/georgianLower.golden"
       ; let chr1 = encodeFromGeorgian inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1        -- make upper case
       ; let txt2 = decodeToGeorgianTitle chr2 -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

georgianCaseCompare :: TestTree
georgianCaseCompare = 
  goldenVsString
    "Mkhedruli -> Mtavruli -> Mkhedruli"
    "golden/georgianLower.golden"
    do { inp <- TU.readFile "golden/georgianUpper.golden"
       ; let chr1 = encodeFromGeorgian inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1        -- make lower case
       ; let txt2 = decodeToGeorgianTitle chr2 -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGeorgianViaGrubb :: TestTree
checkGeorgianViaGrubb = 
  goldenVsString
    "UC: Georgian -> Grubb -> Georgian"
    "golden/georgianUpper.golden"
    do { inp <- TU.readFile "golden/georgianUpper.golden"
       ; let txt1 = decodeToGrubbAscii    $ encodeFromGeorgian     inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGeorgianViaBoas :: TestTree
checkGeorgianViaBoas = 
  goldenVsString
    "UC: Georgian -> Boas  -> Georgian"
    "golden/georgianUpper.golden"
    do { inp <- TU.readFile "golden/georgianUpper.golden"
       ; let txt1 = decodeToPseudoBoas    $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGeorgianViaUmista :: TestTree
checkGeorgianViaUmista = 
  goldenVsString
    "UC: Georgian -> U'mista -> Georgian"
    "golden/georgianUpper.golden"
    do { inp <- TU.readFile "golden/georgianUpper.golden"
       ; let txt1 = decodeToUmista        $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }



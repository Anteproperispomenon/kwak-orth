module Test.Golden.Casing.Umista 
  ( umistaAllLower
  , umistaAllUpper
  , umistaCaseCompare
  , checkUmistaViaGrubb
  , checkUmistaViaBoas
  , checkUmistaViaGeorgian
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

umistaAllLower :: TestTree
umistaAllLower = 
  goldenVsString
    "Make Umista Lowercase"
    "golden/umistaLower.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umistaAllUpper :: TestTree
umistaAllUpper = 
  goldenVsString
    "Make Umista Uppercase"
    "golden/umistaUpper.golden"
    do { inp <- TU.readFile "golden/umistaLower.golden"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1      -- make upper case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umistaCaseCompare :: TestTree
umistaCaseCompare = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    "golden/umistaLower.golden"
    do { inp <- TU.readFile "golden/umistaUpper.golden"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkUmistaViaGrubb :: TestTree
checkUmistaViaGrubb = 
  goldenVsString
    "UC: U'mista -> Grubb -> U'mista"
    "golden/umistaUpper.golden"
    do { inp <- TU.readFile "golden/umistaUpper.golden"
       ; let txt1 = decodeToGrubbAscii $ encodeFromUmista     inp
       ; let txt2 = decodeToUmista     $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkUmistaViaBoas :: TestTree
checkUmistaViaBoas = 
  goldenVsString
    "UC: U'mista -> Boas  -> U'mista"
    "golden/umistaUpper.golden"
    do { inp <- TU.readFile "golden/umistaUpper.golden"
       ; let txt1 = decodeToPseudoBoas $ encodeFromUmista inp
       ; let txt2 = decodeToUmista     $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkUmistaViaGeorgian :: TestTree
checkUmistaViaGeorgian = 
  goldenVsString
    "UC: U'mista -> Georgian -> U'mista"
    "golden/umistaUpper.golden"
    do { inp <- TU.readFile "golden/umistaUpper.golden"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromUmista   inp
       ; let txt2 = decodeToUmista        $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }



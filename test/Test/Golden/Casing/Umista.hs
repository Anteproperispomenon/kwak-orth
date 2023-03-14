module Test.Golden.Casing.Umista 
  ( umistaCaseTests
  , umistaAllLower
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

umistaCaseTests :: TestName -> String -> String -> TestTree
umistaCaseTests tstNam inFile outExt = testGroup ("U'mista Case Tests: " ++ tstNam)
  [ umistaAllLower inFile outExt
  , umistaAllUpper outExt
  , umistaCaseCompare outExt
  , checkUmistaViaGrubb outExt
  , checkUmistaViaBoas outExt
  , checkUmistaViaGeorgian outExt
  ]

umistaAllLower :: String -> String -> TestTree
umistaAllLower inFile outExt = 
  goldenVsString
    "Make Umista Lowercase"
    ("golden/umistaLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umistaAllUpper :: String -> TestTree
umistaAllUpper outExt = 
  goldenVsString
    "Make Umista Uppercase"
    ("golden/umistaUpper" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/umistaLower" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1      -- make upper case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umistaCaseCompare :: String -> TestTree
umistaCaseCompare outExt = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    ("golden/umistaLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/umistaUpper" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkUmistaViaGrubb :: String -> TestTree
checkUmistaViaGrubb outExt = 
  goldenVsString
    "UC: U'mista -> Grubb -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGrubbAscii $ encodeFromUmista     inp
       ; let txt2 = decodeToUmista     $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/umistaUpper" ++ "_" ++ outExt ++ ".golden"

checkUmistaViaBoas :: String -> TestTree
checkUmistaViaBoas outExt = 
  goldenVsString
    "UC: U'mista -> Boas  -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromUmista inp
       ; let txt2 = decodeToUmista     $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/umistaUpper" ++ "_" ++ outExt ++ ".golden"

checkUmistaViaGeorgian :: String -> TestTree
checkUmistaViaGeorgian outExt = 
  goldenVsString
    "UC: U'mista -> Georgian -> U'mista"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromUmista   inp
       ; let txt2 = decodeToUmista        $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/umistaUpper" ++ "_" ++ outExt ++ ".golden"

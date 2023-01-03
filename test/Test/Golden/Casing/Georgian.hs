module Test.Golden.Casing.Georgian
  ( georgianCaseTests
  , georgianAllLower
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

georgianCaseTests :: TestName -> String -> String -> TestTree
georgianCaseTests tstNam inFile outExt = testGroup ("Grubb Case Tests: " ++ tstNam)
  [ georgianAllLower inFile outExt
  , georgianAllUpper outExt
  , georgianCaseCompare outExt
  , checkGeorgianViaGrubb outExt
  , checkGeorgianViaUmista outExt
  , checkGeorgianViaBoas outExt
  ]

georgianAllLower :: String -> String -> TestTree
georgianAllLower inFile outExt = 
  goldenVsString
    "Make Georgian Lowercase"
    ("golden/georgianLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGeorgianTitle chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

georgianAllUpper :: String -> TestTree
georgianAllUpper outExt = 
  goldenVsString
    "Make Georgian Uppercase"
    ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/georgianLower" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromGeorgian inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1        -- make upper case
       ; let txt2 = decodeToGeorgianTitle chr2 -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

georgianCaseCompare :: String -> TestTree
georgianCaseCompare outExt = 
  goldenVsString
    "Mkhedruli -> Mtavruli -> Mkhedruli"
    ("golden/georgianLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromGeorgian inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1        -- make lower case
       ; let txt2 = decodeToGeorgianTitle chr2 -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

--------------------------------------------------------
-- Note: can't compare georgianUpper.golden directly, --
-- since other orthographies can't handle upper-case  --
-- glottal stops at the the end of a word.            --
--------------------------------------------------------

checkGeorgianViaGrubb :: String -> TestTree
checkGeorgianViaGrubb outExt = 
  goldenVsString
    "UC: Georgian -> Grubb -> Georgian"
    ("golden/georgianUpperX" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
       ; let txt1 = decodeToGrubbAscii    $ encodeFromGeorgian   inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGeorgianViaBoas :: String -> TestTree
checkGeorgianViaBoas outExt = 
  goldenVsString
    "UC: Georgian -> Boas  -> Georgian"
    ("golden/georgianUpperX" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
       ; let txt1 = decodeToPseudoBoas    $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGeorgianViaUmista :: String -> TestTree
checkGeorgianViaUmista outExt = 
  goldenVsString
    "UC: Georgian -> U'mista -> Georgian"
    ("golden/georgianUpperX" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
       ; let txt1 = decodeToUmista        $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

-- ("golden/georgianLower" ++ "_" ++ outExt ++ ".golden")
-- ("golden/georgianUpper" ++ "_" ++ outExt ++ ".golden")
-- ("golden/georgianUpperX" ++ "_" ++ outExt ++ ".golden")

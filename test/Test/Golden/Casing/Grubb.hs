module Test.Golden.Casing.Grubb
  ( grubbCaseTests
  , grubbAllLower
  , grubbAllUpper
  , grubbCaseCompare
  , checkGrubbViaUmista
  , checkGrubbViaBoas
  , checkGrubbViaGeorgian
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


grubbCaseTests :: TestName -> String -> String -> TestTree
grubbCaseTests tstNam inFile outExt = testGroup ("Grubb Case Tests: " ++ tstNam)
  [ grubbAllLower inFile outExt
  , grubbAllUpper outExt
  , grubbCaseCompare outExt
  , checkGrubbViaUmista outExt
  , checkGrubbViaBoas outExt
  , checkGrubbViaGeorgian outExt
  ]


grubbAllLower :: String -> String -> TestTree
grubbAllLower inFile outExt = 
  goldenVsString
    "Make Grubb Lowercase"
    ("golden/grubbLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

grubbAllUpper :: String -> TestTree
grubbAllUpper outExt = 
  goldenVsString
    "Make Grubb Uppercase"
    ("golden/grubbUpper" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/grubbLower" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromGrubbAscii inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1      -- make upper case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

grubbCaseCompare :: String -> TestTree
grubbCaseCompare outExt = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    ("golden/grubbLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/grubbUpper" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromGrubbAscii inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGrubbViaUmista :: String -> TestTree
checkGrubbViaUmista outExt = 
  goldenVsString
    "UC: Grubb -> U'mista  -> Grubb"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToUmista     $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromUmista     txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/grubbUpper" ++ "_" ++ outExt ++ ".golden"

checkGrubbViaBoas :: String -> TestTree
checkGrubbViaBoas outExt = 
  goldenVsString
    "UC: Grubb -> Boas     -> Grubb"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/grubbUpper" ++ "_" ++ outExt ++ ".golden"

checkGrubbViaGeorgian :: String -> TestTree
checkGrubbViaGeorgian outExt = 
  goldenVsString
    "UC: Grubb -> Georgian -> Grubb"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii    $ encodeFromGeorgian   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/grubbUpper" ++ "_" ++ outExt ++ ".golden"



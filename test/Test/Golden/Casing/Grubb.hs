module Test.Golden.Casing.Grubb
  ( grubbAllLower
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

grubbAllLower :: TestTree
grubbAllLower = 
  goldenVsString
    "Make Grubb Lowercase"
    "golden/grubbLower.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

grubbAllUpper :: TestTree
grubbAllUpper = 
  goldenVsString
    "Make Grubb Uppercase"
    "golden/grubbUpper.golden"
    do { inp <- TU.readFile "golden/grubbLower.golden"
       ; let chr1 = encodeFromGrubbAscii inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1      -- make upper case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

grubbCaseCompare :: TestTree
grubbCaseCompare = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    "golden/grubbLower.golden"
    do { inp <- TU.readFile "golden/grubbUpper.golden"
       ; let chr1 = encodeFromGrubbAscii inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToGrubbAscii chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGrubbViaUmista :: TestTree
checkGrubbViaUmista = 
  goldenVsString
    "UC: Grubb -> U'mista  -> Grubb"
    "golden/grubbUpper.golden"
    do { inp <- TU.readFile "golden/grubbUpper.golden"
       ; let txt1 = decodeToUmista     $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromUmista     txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGrubbViaBoas :: TestTree
checkGrubbViaBoas = 
  goldenVsString
    "UC: Grubb -> Boas     -> Grubb"
    "golden/grubbUpper.golden"
    do { inp <- TU.readFile "golden/grubbUpper.golden"
       ; let txt1 = decodeToPseudoBoas $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromBoas   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkGrubbViaGeorgian :: TestTree
checkGrubbViaGeorgian = 
  goldenVsString
    "UC: Grubb -> Georgian -> Grubb"
    "golden/grubbUpper.golden"
    do { inp <- TU.readFile "golden/grubbUpper.golden"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii    $ encodeFromGeorgian   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }



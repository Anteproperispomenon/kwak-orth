module Test.Golden.Casing.Napa
  ( napaAllLower
  , napaAllUpper
  , napaCaseCompare
  , checkNapaViaUmista
  , checkNapaViaGrubb
  , checkNapaViaBoas
  , checkNapaViaGeorgian
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

napaAllLower :: TestTree
napaAllLower = 
  goldenVsString
    "Make NAPA Lowercase"
    "golden/napaLower.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars from U'mista
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToNapa chr2    -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

napaAllUpper :: TestTree
napaAllUpper = 
  goldenVsString
    "Make NAPA Uppercase"
    "golden/napaUpper.golden"
    do { inp <- TU.readFile "golden/napaLower.golden"
       ; let chr1 = encodeFromNapa inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1    -- make upper case
       ; let txt2 = decodeToNapa chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

napaCaseCompare :: TestTree
napaCaseCompare = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    "golden/napaLower.golden"
    do { inp <- TU.readFile "golden/napaUpper.golden"
       ; let chr1 = encodeFromNapa inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1    -- make lower case
       ; let txt2 = decodeToNapa chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkNapaViaGrubb :: TestTree
checkNapaViaGrubb = 
  goldenVsString
    "UC: NAPA -> Grubb    -> NAPA"
    "golden/napaUpper.golden"
    do { inp <- TU.readFile "golden/napaUpper.golden"
       ; let txt1 = decodeToGrubbAscii $ encodeFromNapa       inp
       ; let txt2 = decodeToNapa       $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkNapaViaUmista :: TestTree
checkNapaViaUmista = 
  goldenVsStringDiff'
    "UC: NAPA -> U'mista  -> NAPA"
    "golden/napaUpper.golden"
    do { inp <- TU.readFile "golden/napaUpper.golden"
       ; let txt1 = decodeToUmista $ encodeFromNapa   inp
       ; let txt2 = decodeToNapa   $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkNapaViaBoas :: TestTree
checkNapaViaBoas = 
  goldenVsStringDiff'
    "UC: NAPA -> Boas     -> NAPA"
    "golden/napaUpper.golden"
    do { inp <- TU.readFile "golden/napaUpper.golden"
       ; let txt1 = decodeToPseudoBoas $ encodeFromNapa inp
       ; let txt2 = decodeToNapa       $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkNapaViaGeorgian :: TestTree
checkNapaViaGeorgian = 
  goldenVsStringDiff'
    "UC: NAPA -> Georgian -> NAPA"
    "golden/fixedNapa.golden"
    do { inp <- TU.readFile "examples/sample1_napa.txt"
       ; let txt1 = decodeToGeorgianTitle $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa          $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }


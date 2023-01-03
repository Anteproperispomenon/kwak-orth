module Test.Golden.Casing.Napa
  ( napaCaseTests
  , napaAllLower
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


napaCaseTests :: TestName -> String -> String -> TestTree
napaCaseTests tstNam inFile outExt = testGroup ("NAPA Case Tests: " ++ tstNam)
  [ napaAllLower inFile outExt
  , napaAllUpper outExt
  , napaCaseCompare outExt
  , checkNapaViaUmista outExt
  , checkNapaViaGrubb outExt
  , checkNapaViaBoas outExt
  , checkNapaViaGeorgian outExt
  ]

napaAllLower :: String -> String -> TestTree
napaAllLower inFile outExt = 
  goldenVsString
    "Make NAPA Lowercase"
    ("golden/napaLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars from U'mista
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToNapa chr2    -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

napaAllUpper :: String -> TestTree
napaAllUpper outExt = 
  goldenVsString
    "Make NAPA Uppercase"
    ("golden/napaUpper" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/napaLower" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromNapa inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1    -- make upper case
       ; let txt2 = decodeToNapa chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

napaCaseCompare :: String -> TestTree
napaCaseCompare outExt = 
  goldenVsString
    "Lowercase -> Uppercase -> Lowercase"
    ("golden/napaLower" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile ("golden/napaUpper" ++ "_" ++ outExt ++ ".golden")
       ; let chr1 = encodeFromNapa inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1    -- make lower case
       ; let txt2 = decodeToNapa chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

checkNapaViaGrubb :: String -> TestTree
checkNapaViaGrubb outExt = 
  goldenVsString
    "UC: NAPA -> Grubb    -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGrubbAscii $ encodeFromNapa       inp
       ; let txt2 = decodeToNapa       $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/napaUpper" ++ "_" ++ outExt ++ ".golden"

checkNapaViaUmista :: String -> TestTree
checkNapaViaUmista outExt = 
  goldenVsStringDiff'
    "UC: NAPA -> U'mista  -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToUmista $ encodeFromNapa   inp
       ; let txt2 = decodeToNapa   $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/napaUpper" ++ "_" ++ outExt ++ ".golden"

checkNapaViaBoas :: String -> TestTree
checkNapaViaBoas outExt = 
  goldenVsStringDiff'
    "UC: NAPA -> Boas     -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromNapa inp
       ; let txt2 = decodeToNapa       $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/napaUpper" ++ "_" ++ outExt ++ ".golden"

checkNapaViaGeorgian :: String -> TestTree
checkNapaViaGeorgian outExt = 
  goldenVsStringDiff'
    "UC: NAPA -> Georgian -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa          $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/napaUpper" ++ "_" ++ outExt ++ ".golden"

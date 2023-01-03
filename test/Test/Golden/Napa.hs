module Test.Golden.Napa
  ( fixNapaTests
  , fixNapaTest
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

import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU

fixNapaTests :: TestName -> String -> String -> TestTree
fixNapaTests tstNam inFile outExt = testGroup ("NAPA Tests: " ++ tstNam)
  [ fixNapaTest inFile outExt
  , fixNapaViaGrubbTest outExt
  , fixNapaViaUmistaTest outExt
  , fixNapaViaBoasTest outExt
  , fixNapaViaGeorgianTest outExt
  ]

fixNapaTest :: String -> String -> TestTree
fixNapaTest inFile outExt = 
  goldenVsString
    "Fix NAPA"
    ("golden/fixedNapa" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt = decodeToNapa $ encodeFromNapa inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixNapaViaGrubbTest :: String -> TestTree
fixNapaViaGrubbTest outExt = 
  goldenVsStringDiff'
    "NAPA -> Grubb    -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGrubbAscii $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa       $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedNapa" ++ "_" ++ outExt ++ ".golden"

fixNapaViaUmistaTest :: String -> TestTree
fixNapaViaUmistaTest outExt = 
  goldenVsStringDiff'
    "NAPA -> U'mista  -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToUmista $ encodeFromNapa   inp
       ; let txt2 = decodeToNapa   $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedNapa" ++ "_" ++ outExt ++ ".golden"

fixNapaViaBoasTest :: String -> TestTree
fixNapaViaBoasTest outExt = 
  goldenVsStringDiff'
    "NAPA -> Boas     -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromNapa inp
       ; let txt2 = decodeToNapa       $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedNapa" ++ "_" ++ outExt ++ ".golden"

fixNapaViaGeorgianTest :: String -> TestTree
fixNapaViaGeorgianTest outExt = 
  goldenVsStringDiff'
    "NAPA -> Georgian -> NAPA"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromNapa     inp
       ; let txt2 = decodeToNapa          $ encodeFromGeorgian txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedNapa" ++ "_" ++ outExt ++ ".golden"

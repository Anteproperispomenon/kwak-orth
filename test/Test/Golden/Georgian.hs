module Test.Golden.Georgian
  ( fixGeorgianTests
  , fixGeorgianTest
  , fixGeorgianViaGrubbTest
  , fixGeorgianViaUmistaTest
  , fixGeorgianViaBoasTest
  , fixGeorgianViaNapaTest
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

fixGeorgianTests :: TestName -> String -> String -> TestTree
fixGeorgianTests tstNam inFile outExt = testGroup ("Georgian Tests: " ++ tstNam)
  [ fixGeorgianTest inFile outExt
  , georgianCustomTest1 inFile outExt
  , georgianCustomTest2 outExt
  , fixGeorgianViaGrubbTest outExt
  , fixGeorgianViaUmistaTest outExt
  , fixGeorgianViaBoasTest outExt
  , fixGeorgianViaNapaTest outExt
  ]

fixGeorgianTest :: String -> String -> TestTree
fixGeorgianTest inFile outExt = 
  goldenVsString
    "Fix Georgian"
    ("golden/fixedGeorgian" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt = decodeToGeorgianTitle $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixGeorgianViaGrubbTest :: String -> TestTree
fixGeorgianViaGrubbTest outExt = 
  goldenVsString
    "Georgian -> Grubb   -> Georgian"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToGrubbAscii    $ encodeFromGeorgian   inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedGeorgian" ++ "_" ++ outExt ++ ".golden"

fixGeorgianViaUmistaTest :: String -> TestTree
fixGeorgianViaUmistaTest outExt = 
  goldenVsString
    "Georgian -> U'mista -> Georgian"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToUmista        $ encodeFromGeorgian   inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromUmista     txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedGeorgian" ++ "_" ++ outExt ++ ".golden"

fixGeorgianViaBoasTest :: String -> TestTree
fixGeorgianViaBoasTest outExt = 
  goldenVsString
    "Georgian -> Boas    -> Georgian"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromBoas txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedGeorgian" ++ "_" ++ outExt ++ ".golden"

fixGeorgianViaNapaTest :: String -> TestTree
fixGeorgianViaNapaTest outExt = 
  goldenVsString
    "Georgian -> NAPA    -> Georgian"
    goldFile
    do { inp <- TU.readFile goldFile
       ; let txt1 = decodeToNapa $ encodeFromGeorgian inp
       ; let txt2 = decodeToGeorgianTitle $ encodeFromNapa txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }
  where goldFile = "golden/fixedGeorgian" ++ "_" ++ outExt ++ ".golden"

georgianCustomTest1 :: String -> String -> TestTree
georgianCustomTest1 inFile outExt = 
  goldenVsString
    "Output Alt Georgian"
    ("golden/fixedGeorgianC" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt = decodeToGeorgianC fullCustom $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

georgianCustomTest2 :: String -> TestTree
georgianCustomTest2 outExt = 
  goldenVsString
    "Parse Alt Georgian"
    goldFileO
    do { inp <- TU.readFile goldFileI
       ; let txt1 = decodeToGeorgianTitle $ encodeFromGeorgian inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt1
       }
  where goldFileI = "golden/fixedGeorgianC" ++ "_" ++ outExt ++ ".golden"
        goldFileO = "golden/fixedGeorgian"  ++ "_" ++ outExt ++ ".golden"
         

fullCustom :: GeorgianOutputConfig
fullCustom = GeorgianOutputConfig True True

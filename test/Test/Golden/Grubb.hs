module Test.Golden.Grubb
  ( fixGrubbTests
  , fixGrubbTest
  , fixGrubbViaNapaTest
  , fixGrubbViaUmistaTest
  , fixGrubbViaBoasTest
  , fixGrubbViaGeorgianTest
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

fixGrubbTests :: TestName -> String -> String -> TestTree
fixGrubbTests tstNam inFile outExt = testGroup ("Grubb Tests: " ++ tstNam)
  [ fixGrubbTest inFile outExt
  , fixGrubbViaGeorgianTest inFile outExt
  , fixGrubbViaUmistaTest inFile outExt
  , fixGrubbViaBoasTest inFile outExt
  , fixGrubbViaNapaTest inFile outExt
  ]

fixGrubbTest :: String -> String -> TestTree
fixGrubbTest inFile outExt = 
  goldenVsString
    "Fix Grubb"
    ("golden/fixedGrubb" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt = decodeToGrubbAscii $ encodeFromGrubbAscii inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixGrubbViaNapaTest :: String -> String -> TestTree
fixGrubbViaNapaTest inFile outExt = 
  goldenVsString
    "Grubb -> NAPA     -> Grubb"
    ("golden/fixedGrubb" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt1 = decodeToNapa       $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromNapa       txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaUmistaTest :: String -> String -> TestTree
fixGrubbViaUmistaTest inFile outExt = 
  goldenVsString
    "Grubb -> U'mista  -> Grubb"
    ("golden/fixedGrubb" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt1 = decodeToUmista $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromUmista txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaBoasTest :: String -> String -> TestTree
fixGrubbViaBoasTest inFile outExt = 
  goldenVsString
    "Grubb -> Boas     -> Grubb"
    ("golden/fixedGrubb" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt1 = decodeToPseudoBoas $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii $ encodeFromBoas       txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

fixGrubbViaGeorgianTest :: String -> String -> TestTree
fixGrubbViaGeorgianTest inFile outExt = 
  goldenVsString
    "Grubb -> Georgian -> Grubb"
    ("golden/fixedGrubb" ++ "_" ++ outExt ++ ".golden")
    do { inp <- TU.readFile inFile
       ; let txt1 = decodeToGeorgianTitle $ encodeFromGrubbAscii inp
       ; let txt2 = decodeToGrubbAscii    $ encodeFromGeorgian   txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

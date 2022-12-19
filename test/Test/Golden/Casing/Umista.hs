module Test.Golden.Casing.Umista 
  ( umistaAllLower
  , umistaAllUpper
  , umistaCaseCompare
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
  goldenVsStringDiff'
    "Make Umista Uppercase"
    "golden/umistaUpper.golden"
    do { inp <- TU.readFile "golden/umistaLower.golden"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMaj' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umistaCaseCompare :: TestTree
umistaCaseCompare = 
  goldenVsStringDiff'
    "Lowercase -> Uppercase -> Lowercase"
    "golden/umistaLower.golden"
    do { inp <- TU.readFile "golden/umistaUpper.golden"
       ; let chr1 = encodeFromUmista inp -- convert to CasedChars
       ; let chr2 = map toMin' chr1      -- make lower case
       ; let txt2 = decodeToUmista chr2  -- decode to text
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }


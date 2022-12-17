module Test.Golden.Umista 
  ( fixUmistaTest
  , fixUmistaViaGrubbTest
  , umista2NapaTest
  ) where

-- Testing Input to/from U'mista.

import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.Golden

import Data.ByteString.Lazy qualified as BL

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import Data.Text.Lazy          qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

import Kwakwala.Output
import Kwakwala.Parsers

import TextUTF8 qualified as TU



fixUmistaTest :: TestTree
fixUmistaTest = 
  goldenVsString
    "Fix Umista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt = decodeToUmista $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

fixUmistaViaGrubbTest :: TestTree
fixUmistaViaGrubbTest = 
  goldenVsStringDiff'
    "Umista -> Grubb -> Umista"
    "golden/fixedUmista.golden"
    do { inp <- TU.readFile "examples/sample1_umista_raw.txt"
       ; let txt1 = decodeToGrubbAscii $ encodeFromUmista     inp
       ; let txt2 = decodeToUmista     $ encodeFromGrubbAscii txt1
       ; return $ BL.fromStrict $ T.encodeUtf8 txt2
       }

umista2NapaTest :: TestTree
umista2NapaTest = 
  goldenVsString
    "Umista to NAPA"
    "golden/Umista2Napa.golden"
    do { inp <- TU.readFile "examples/sample1_umista.txt"
       ; let txt = decodeToNapa $ encodeFromUmista inp
       ; return $ BL.fromStrict $ T.encodeUtf8 txt
       }

-- | Drop-in replacement for `goldenVsString`
-- but using diff.
goldenVsStringDiff' :: TestName -> FilePath -> IO BL.ByteString -> TestTree
goldenVsStringDiff' tstN fp bs
  = goldenVsStringDiff tstN (\ref new -> ["diff", "-u", ref, new]) fp bs

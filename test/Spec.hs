
import Test.Tasty (defaultMain, TestTree, testGroup)

import Test.Golden.Umista qualified as Umista
import Test.Golden.Napa   qualified as Napa
import Test.Golden.Grubb  qualified as Grubb

import Test.Golden.Casing.Umista qualified as UmistaCase
import Test.Golden.Casing.Napa   qualified as NapaCase

import TextUTF8 qualified as TU

main :: IO ()
main = TU.fixLocale >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Golden Tests"
    [ testGroup "U'mista"
      [ testGroup "Parsing" 
        [ Umista.fixUmistaTest
        , Umista.fixUmistaViaGrubbTest
        , Umista.fixUmistaViaNapaTest
        , Umista.fixUmistaViaBoasTest
        , Umista.fixUmistaViaGeorgianTest
        , Umista.umista2NapaTest
        ]
      , testGroup "Casing"
        [ UmistaCase.umistaAllLower
        , UmistaCase.umistaAllUpper
        , UmistaCase.umistaCaseCompare
        , UmistaCase.checkUmistaViaGrubb
        , UmistaCase.checkUmistaViaBoas
        , UmistaCase.checkUmistaViaGeorgian
        ]
      ]
    , testGroup "NAPA"
        [ testGroup "Parsing"
          [ Napa.fixNapaTest
          , Napa.fixNapaViaGrubbTest
          , Napa.fixNapaViaUmistaTest
          , Napa.fixNapaViaBoasTest
          , Napa.fixNapaViaGeorgianTest
          ]
        , testGroup "Casing"
          [ NapaCase.napaAllLower
          , NapaCase.napaAllUpper
          , NapaCase.napaCaseCompare
          , NapaCase.checkNapaViaUmista
          , NapaCase.checkNapaViaGrubb
          , NapaCase.checkNapaViaBoas
          , NapaCase.checkNapaViaGeorgian
          ]
        ]
    , testGroup "Grubb"
        [ Grubb.fixGrubbTest
        , Grubb.fixGrubbViaNapaTest
        , Grubb.fixGrubbViaUmistaTest
        , Grubb.fixGrubbViaBoasTest
        , Grubb.fixGrubbViaGeorgianTest
        ]
    ]
  ]

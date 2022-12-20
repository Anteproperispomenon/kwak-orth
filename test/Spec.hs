
import Test.Tasty (defaultMain, TestTree, testGroup)

import Test.Golden.Umista   qualified as Umista
import Test.Golden.Napa     qualified as Napa
import Test.Golden.Grubb    qualified as Grubb
import Test.Golden.Georgian qualified as Georgian

import Test.Golden.Casing.Umista   qualified as UmistaCase
import Test.Golden.Casing.Napa     qualified as NapaCase
import Test.Golden.Casing.Grubb    qualified as GrubbCase
import Test.Golden.Casing.Georgian qualified as GeorgianCase

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
        [ testGroup "Parsing"
          [ Grubb.fixGrubbTest
          , Grubb.fixGrubbViaNapaTest
          , Grubb.fixGrubbViaUmistaTest
          , Grubb.fixGrubbViaBoasTest
          , Grubb.fixGrubbViaGeorgianTest
          ]
        , testGroup "Casing"
          [ GrubbCase.grubbAllLower
          , GrubbCase.grubbAllUpper
          , GrubbCase.grubbCaseCompare
          , GrubbCase.checkGrubbViaUmista
          , GrubbCase.checkGrubbViaBoas
          , GrubbCase.checkGrubbViaGeorgian
          ]
        ]
    , testGroup "Georgian"
        [ testGroup "Parsing"
          [ Georgian.fixGeorgianTest
          , Georgian.fixGeorgianViaNapaTest
          , Georgian.fixGeorgianViaUmistaTest
          , Georgian.fixGeorgianViaBoasTest
          , Georgian.fixGeorgianViaGrubbTest
          ]
        , testGroup "Casing"
          [ GeorgianCase.georgianAllLower
          , GeorgianCase.georgianAllUpper
          , GeorgianCase.georgianCaseCompare
          , GeorgianCase.checkGeorgianViaUmista
          , GeorgianCase.checkGeorgianViaBoas
          , GeorgianCase.checkGeorgianViaGrubb
          ] 
        ]
    ]
  ]


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
    [ testGroup "Parsing/Idempotence"
      [ Umista.fixUmistaTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , Napa.fixNapaTests "Original File" "examples/sample1_napa.txt" "orig"
      , Grubb.fixGrubbTests "Original File" "examples/sample1_grubb.txt" "orig"
      , Georgian.fixGeorgianTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      ]
    , testGroup "Casing"
      [ UmistaCase.umistaCaseTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , NapaCase.napaCaseTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , GrubbCase.grubbCaseTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , GeorgianCase.georgianCaseTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      ]
    ]
  ]

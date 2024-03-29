
import Test.Tasty (defaultMain, TestTree, testGroup, localOption)
import Test.Tasty.Golden (SizeCutoff)

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
main = TU.fixLocale >> defaultMain (localOption (256 :: SizeCutoff) tests)

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Golden Tests"
    [ testGroup "Parsing/Idempotence"
      [ Umista.fixUmistaTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , Umista.fixUmistaTests "New File" "examples/sample2_umista.txt" "new"
      , Napa.fixNapaTests "Original File" "examples/sample1_napa.txt" "orig"
      , Grubb.fixGrubbTests "Original File" "examples/sample1_grubb.txt" "orig"
      , Georgian.fixGeorgianTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , Georgian.fixGeorgianTests "New File" "examples/sample2_umista.txt" "new"
      ]
    , testGroup "Casing"
      [ UmistaCase.umistaCaseTests     "Original File" "examples/sample1_umista_raw.txt" "orig"
      , NapaCase.napaCaseTests         "Original File" "examples/sample1_umista_raw.txt" "orig"
      , GrubbCase.grubbCaseTests       "Original File" "examples/sample1_umista_raw.txt" "orig"
      , GeorgianCase.georgianCaseTests "Original File" "examples/sample1_umista_raw.txt" "orig"
      , UmistaCase.umistaCaseTests     "New File" "examples/sample2_umista.txt" "new"
      , NapaCase.napaCaseTests         "New File" "examples/sample2_umista.txt" "new"
      , GrubbCase.grubbCaseTests       "New File" "examples/sample2_umista.txt" "new"
      , GeorgianCase.georgianCaseTests "New File" "examples/sample2_umista.txt" "new"
      ]
    ]
  ]

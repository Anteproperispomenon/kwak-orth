
import Test.Tasty (defaultMain, TestTree, testGroup)

import Test.Golden.Umista qualified as Umista

import TextUTF8 qualified as TU

main :: IO ()
main = TU.fixLocale >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Golden Tests"
    [ testGroup "U'mista"
        [ Umista.fixUmistaTest
        , Umista.fixUmistaViaGrubbTest
        , Umista.umista2NapaTest
        ]
    ]
  ]
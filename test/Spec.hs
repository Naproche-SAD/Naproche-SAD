{-
  Start the test procedure by running `stack test` in
  the project directory. To load the test modules in
  the REPL run `stack repl Naproche-SAD:Naproche-SAD-test`.
-}

import Test.Hspec

import qualified TokenSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tokenizer" TokenSpec.spec
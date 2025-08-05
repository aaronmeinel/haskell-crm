
import Test.Hspec
import ListConstituentsSpec
import AddConstituentSpec
import LetterGeneratorSpec

main :: IO ()
main = hspec $ do
  ListConstituentsSpec.spec
  AddConstituentSpec.spec
  LetterGeneratorSpec.spec

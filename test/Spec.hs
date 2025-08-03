
import Test.Hspec
import ListConstituentsSpec
import AddConstituentSpec

main :: IO ()
main = hspec $ do
  ListConstituentsSpec.spec
  AddConstituentSpec.spec

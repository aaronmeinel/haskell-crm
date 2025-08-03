import Test.Hspec
import ListConstituentsSpec

main :: IO ()
main = hspec $ do
  ListConstituentsSpec.spec

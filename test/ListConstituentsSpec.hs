
module ListConstituentsSpec (spec) where

import Test.Hspec
import Domain.Constituent
import App.ListConstituents (listConstituents)

spec :: Spec
spec = describe "listConstituents" $ do
  it "returns all constituents (in-memory, free monad)" $ do
    let store = [ Constituent "Alice" "alice@example.com"
               , Constituent "Bob" "bob@example.com"
               ]
        cs = listConstituents store
    cs `shouldBe` store

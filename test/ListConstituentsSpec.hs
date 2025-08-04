

module ListConstituentsSpec (spec) where

import Test.Hspec
import Domain.Constituent
import App.ListConstituents (listConstituents)
import App.ConstituentRepoInMemory ()
import Control.Monad.State

spec :: Spec
spec = describe "listConstituents" $ do
  it "returns all constituents (in-memory, MTL, typeclass)" $ do
    let store = [ Constituent "Alice" "alice@example.com"
               , Constituent "Bob" "bob@example.com"
               ]
        cs = evalState listConstituents store

    cs `shouldBe` store

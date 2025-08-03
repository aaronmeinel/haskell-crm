
module AddConstituentSpec (spec) where

import Test.Hspec
import Domain.Constituent
import App.AddConstituent (addConstituentAndList)
import App.ConstituentRepoInMemory ()
import Control.Monad.State

spec :: Spec
spec = describe "addConstituent" $ do
  it "adds a constituent to the list (in-memory, MTL)" $ do
    let newC = Constituent "Charlie" "charlie@example.com"
        cs = evalState (addConstituentAndList newC) ([] :: [Constituent])
    cs `shouldContain` [newC]

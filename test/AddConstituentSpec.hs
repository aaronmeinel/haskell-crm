module AddConstituentSpec (spec) where

import Test.Hspec
import Domain.Constituent
import App.AddConstituent (addConstituent)

spec :: Spec
spec = describe "addConstituent" $ do
  it "adds a constituent to the list (in-memory stub)" $ do
    let newC = Constituent "Charlie" "charlie@example.com"
    cs <- addConstituent newC
    cs `shouldContain` [newC]

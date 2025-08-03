
module ListConstituentsSpec (spec) where

import Test.Hspec
import Domain.Constituent
import App.ListConstituents (listConstituents)

spec :: Spec
spec = describe "listConstituents" $ do
  it "returns all constituents (sample data)" $ do
    cs <- listConstituents
    cs `shouldBe`
      [ Constituent "Alice" "alice@example.com"
      , Constituent "Bob" "bob@example.com"
      ]

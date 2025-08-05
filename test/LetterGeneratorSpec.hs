module LetterGeneratorSpec (spec) where

import Test.Hspec
import App.LetterGenerator (generateLetterForConstituent)
import Domain.Constituent (Constituent(..))
import qualified Data.Text as T

template :: T.Text
template = T.pack "Dear {{name}}, your email is {{email}}."

spec :: Spec
spec = describe "generateLetterForConstituent" $ do
  it "renders a letter with name and email" $ do
    let c = Constituent { name = "Alice", email = "alice@example.com" }
        result = generateLetterForConstituent template c
    result `shouldBe` Right (T.pack "Dear Alice, your email is alice@example.com.")

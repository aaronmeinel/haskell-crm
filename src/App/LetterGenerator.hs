
{-# LANGUAGE OverloadedStrings #-}
module App.LetterGenerator (generateLetterForConstituent) where

import Domain.Constituent (Constituent(..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Text.Mustache as M
import qualified Data.Aeson as A

-- | Generate a letter for a constituent using a Mustache template (as Text)
--   Returns either an error message or the rendered letter as Text
--   Usage: generateLetterForConstituent templateText constituent

generateLetterForConstituent :: T.Text -> Constituent -> Either String T.Text
generateLetterForConstituent templateTxt c =
  case M.compileTemplate "user-template" templateTxt of
    Left err -> Left (show err)
    Right template ->
      let ctx = A.object [ "name" A..= name c, "email" A..= email c ] -- Create json context, which needs these weird A..= operators
      in Right $ M.substitute template ctx

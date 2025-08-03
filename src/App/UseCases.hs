module App.UseCases where

import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepo

addAndList :: MonadConstituentRepo m => Constituent -> m [Constituent]
addAndList c = do
  addConstituent c
  listConstituents

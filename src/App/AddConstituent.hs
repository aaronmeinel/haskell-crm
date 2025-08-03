module App.AddConstituent (addConstituentAndList) where

import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepo

-- | Add a constituent and return the updated list using the repository typeclass
addConstituentAndList :: MonadConstituentRepo m => Constituent -> m [Constituent]
addConstituentAndList c = do
  addConstituent c
  listConstituents

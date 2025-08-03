module App.AddConstituent (addConstituent) where

import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepoF
import App.ConstituentRepoInterpreter (runInMemory)

-- | Use the free monad to add a constituent and return the new list (in-memory)
addConstituent :: Constituent -> [Constituent] -> [Constituent]
addConstituent c store = runInMemory store (addConstituentF c)

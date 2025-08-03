module App.ConstituentRepoInterpreter (runInMemory) where

import Domain.Constituent (Constituent)
import Domain.ConstituentRepoF
import Control.Monad.Free (Free(..))

-- | Simple in-memory interpreter for the ConstituentRepo free monad
runInMemory :: [Constituent] -> ConstituentRepo a -> a
runInMemory store (Pure a) = a
runInMemory store (Free (AddConstituent c next)) =
  let newStore = store ++ [c]
  in runInMemory newStore (next newStore)
runInMemory store (Free (ListConstituents next)) =
  runInMemory store (next store)

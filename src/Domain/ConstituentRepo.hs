module Domain.ConstituentRepo where

import Domain.Constituent (Constituent)

class Monad m => MonadConstituentRepo m where
  addConstituent :: Constituent -> m ()
  listConstituents :: m [Constituent]

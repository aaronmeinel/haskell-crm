module App.AddConstituent (addConstituentAndList) where

import Domain.Constituent (Constituent(..))
import qualified Data.UUID.V4 as UUIDv4
import Domain.ConstituentRepo
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Add a constituent and return the updated list using the repository typeclass

addConstituentAndList :: (MonadConstituentRepo m, MonadIO m) => String -> String -> m [Constituent]
addConstituentAndList name email = do
  uuid <- liftIO UUIDv4.nextRandom
  let c = Constituent name email uuid
  addConstituent c
  listConstituents

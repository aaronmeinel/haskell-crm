
module App.ListConstituents (listConstituents) where

import Domain.Constituent (Constituent(..))
import qualified Domain.ConstituentRepo as Repo

-- | List all constituents using the repository typeclass
listConstituents :: Repo.MonadConstituentRepo m => m [Constituent]
listConstituents = Repo.listConstituents

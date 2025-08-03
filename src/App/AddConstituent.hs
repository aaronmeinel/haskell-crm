module App.AddConstituent (addConstituent) where

import Domain.Constituent (Constituent(..))

-- | For now, just returns a list with the new constituent (stub for future DB integration)
addConstituent :: Constituent -> IO [Constituent]
addConstituent c = pure [c]

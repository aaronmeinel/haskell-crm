module App.ListConstituents (listConstituents) where

import Domain.Constituent (Constituent(..))

-- | Use the free monad to list constituents (in-memory)
listConstituents :: [Constituent] -> [Constituent]
listConstituents store = runInMemory store listConstituentsF

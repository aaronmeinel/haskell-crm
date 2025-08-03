module App.ListConstituents (listConstituents) where

import Domain.Constituent (Constituent(..))

-- | For now, returns a static list of constituents (stub for future DB integration)
listConstituents :: IO [Constituent]
listConstituents = pure
  [ Constituent "Alice" "alice@example.com"
  , Constituent "Bob" "bob@example.com"
  ]

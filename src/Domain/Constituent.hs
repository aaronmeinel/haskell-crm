module Domain.Constituent (
    Constituent(..)
) where


import Data.UUID (UUID)

-- | A constituent (person or organization in the CRM)
data Constituent = Constituent
  { name  :: String
  , email :: String
  , uuid :: UUID
  } deriving (Eq, Show)

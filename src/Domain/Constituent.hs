module Domain.Constituent (
    Constituent(..)
) where

-- | A constituent (person or organization in the CRM)
data Constituent = Constituent
  { name  :: String
  , email :: String
  } deriving (Eq, Show)

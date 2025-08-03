{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.ConstituentRepoInMemory where

import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepo
import Control.Monad.State

-- | In-memory instance for MonadConstituentRepo, for testing
instance MonadConstituentRepo (State [Constituent]) where
  addConstituent c = modify (++ [c])
  listConstituents = get

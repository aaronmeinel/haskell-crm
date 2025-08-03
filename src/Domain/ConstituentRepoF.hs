{-# LANGUAGE DeriveFunctor #-}

module Domain.ConstituentRepoF (
    ConstituentRepoF(..),
    ConstituentRepo,
    addConstituentF,
    listConstituentsF
) where

import Domain.Constituent (Constituent)
import Control.Monad.Free (Free, liftF)

-- | Functor for constituent repository operations
 data ConstituentRepoF next
   = AddConstituent Constituent ( [Constituent] -> next )
   | ListConstituents ( [Constituent] -> next )
   deriving Functor

type ConstituentRepo = Free ConstituentRepoF

addConstituentF :: Constituent -> ConstituentRepo [Constituent]
addConstituentF c = liftF (AddConstituent c id)

listConstituentsF :: ConstituentRepo [Constituent]
listConstituentsF = liftF (ListConstituents id)

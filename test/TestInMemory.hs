module TestInMemory where

import App.UseCases
import App.ConstituentRepoInMemory
import Domain.Constituent
import Control.Monad.State

runTest :: [Constituent]
runTest = execState (addAndList (Constituent "Alice" "alice@example.com")) []

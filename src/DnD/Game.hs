module DnD.Game where

import           Control.Monad.Free
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Monoid
import           System.Random

import           DnD.Dice

type GameStateT a = StateT StdGen a

runGameState s x = runStateT x s

runRollerState :: Monad m => Free Roller Int -> GameStateT m Int
runRollerState r = do
  gen <- get
  let (roll, gen') = runRollerPure gen r
  put gen'
  return roll

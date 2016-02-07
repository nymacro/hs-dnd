{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module DnD.Dice where

import           Control.Monad.Free
import           System.Random

data Roller next = D Int next
                 | Plus Int next
                 | Minus Int next
                 deriving (Show)

instance Functor Roller where
  fmap f (D n next)     = D n (f next)
  fmap f (Plus n next)  = Plus n (f next)
  fmap f (Minus n next) = Minus n (f next)

roll n  = liftF (D n 0)
plus n  = liftF (Plus n 0)
minus n = liftF (Minus n 0)

runRoller :: Free Roller Int -> IO Int
runRoller r = do
  seed <- getStdRandom random
  return $ runRollerPure (mkStdGen seed) r

runRollerPure :: StdGen -> Free Roller Int -> Int
runRollerPure g (Pure r) = r
runRollerPure g (Free (D n next)) =
  let (roll, gen) = randomR (1, n) g
  in roll + runRollerPure gen next
runRollerPure g (Free (Plus n next))  = n + runRollerPure g next
runRollerPure g (Free (Minus n next)) = runRollerPure g next - n

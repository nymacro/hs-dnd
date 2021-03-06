{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module DnD.Dice where

import           Control.Monad.Free
import           System.Random
import           Text.Show.Deriving

data Roller next = D Int next
                 | Plus Int next
                 | Minus Int next
                 deriving (Show)
$(deriveShow1 ''Roller) -- why?

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
  return $ fst $ runRollerPure (mkStdGen seed) r

runRollerPure :: StdGen -> Free Roller Int -> (Int, StdGen)
runRollerPure g (Pure r) = (r, g)
runRollerPure g (Free (D n next)) =
  let (roll, gen)   = randomR (1, n) g
      (roll', gen') = runRollerPure gen next
  in (roll + roll', gen')
runRollerPure g (Free (Plus n next))  = let (roll, gen) = runRollerPure g next
                                        in (n + roll, gen)
runRollerPure g (Free (Minus n next)) = let (roll, gen) = runRollerPure g next
                                        in (roll - n, gen)

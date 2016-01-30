{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module DnD.Dice where

import System.Random
import Control.Monad.State

type DiceBag = State StdGen Int

mkDiceBag :: DiceBag
mkDiceBag = return 0 -- (mkStdGen 0)

roll :: Int -> DiceBag
roll d = do
  (n, gen) <- fmap (randomR (1, d)) get
  put gen
  return n

rolls :: DiceBag
rolls = do
  a <- roll 6
  b <- roll 6
  return $ a + b

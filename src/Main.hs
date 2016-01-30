module Main where

import           DnD.Dice
import           DnD.Player

import           Control.Monad.State
import           Data.Monoid
import           System.Random

import           Control.Lens

main :: IO ()
main = do
  print $ runState (do
                    roll 8
                    roll 4) (mkStdGen 0)
  print $ runState rolls (mkStdGen 10)
  putStrLn "hello world"

  rolls <- mkStatsRoll
  print rolls
  let player = mkPlayer { _stats = rolls, _race = orc, _levels = [mkLevelClass barbarian] } -- { _feats = [alertness, combatReflexes]}
  let p = applyFeats . applyLevels . applyRaceBonus $ player
  print p
  print $ modifier (p ^. stats)

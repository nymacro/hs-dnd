{-# LANGUAGE OverloadedStrings #-}
module DnD.Default where

import           DnD.Player
import           DnD.Race

import           Control.Monad
import           Data.List     (sort)
import           System.Random

mkStats :: Stats
mkStats = Stats 0 0 0 0 0 0

mkStatsRoll :: IO Stats
mkStatsRoll = Stats <$> mkStat <*> mkStat <*> mkStat <*> mkStat <*> mkStat <*> mkStat
  -- take sum of highest 3 of 4 D6 rolls
  where mkStat = (sum . drop 1 . sort) <$> replicateM 4 (randomRIO (1, 6) :: IO Int)

mkSkills :: Skills
mkSkills = Skills 0 0 0 0 0 0 0 0

mkLevelClass :: Class -> LevelClass
mkLevelClass = LevelClass 0 0 mkSkills []

mkPaperdoll :: Paperdoll
mkPaperdoll = Paperdoll Nothing Nothing Nothing

mkPlayer :: Player
mkPlayer = Player { _name   = "Player"
                  , _race   = human
                  , _xp     = 0
                  , _hp     = 4
                  , _ac     = 0
                  , _initiative = 0
                  , _attackModifier = Strength
                  , _stats  = mkStats
                  , _skills = mkSkills
                  , _spells = []
                  , _levels = []
                  , _equipped = mkPaperdoll
                  , _feats  = []
                  , _proficiencies = [] }

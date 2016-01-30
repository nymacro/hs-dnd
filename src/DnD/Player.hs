{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module DnD.Player where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.Text

import           System.Random

data Stats = Stats { _strength     :: Int
                   , _dexterity    :: Int
                   , _constitution :: Int
                   , _intelligence :: Int
                   , _wisdom       :: Int
                   , _charisma     :: Int }
           deriving (Show, Eq)

type Modifiers = Stats

data Class = Class { _className :: Text
                   , _hitDie    :: Int
                   , applyClass :: Int -> Player -> Player }

data Skills = Skills { _acrobatics     :: Int
                     , _animalHandling :: Int
                     , _arcana         :: Int
                     , _athletics      :: Int
                     , _deception      :: Int
                     , _history        :: Int
                     , _insight        :: Int
                     , _intimidation   :: Int }
              deriving (Show)

data Race = Race { _raceName :: Text
                 , applyRace :: Player -> Player }

data Feat = Feat { _featName :: Text
                 , applyFeat :: Player -> Player }

data LevelClass = LevelClass { _levelHp     :: Int
                             , _levelStats  :: Int
                             , _levelSkills :: Skills
                             , _levelClass  :: Class }

data Player = Player { _name   :: Text
                     , _race   :: Race
                     , _hp     :: Int
                     , _stats  :: Stats
                     , _ac     :: Int
                     , _skills :: Skills
                     , _levels :: [LevelClass]
                     , _feats  :: [Feat] }

makeLenses ''Player
makeLenses ''Skills
makeLenses ''Stats
makeLenses ''Feat
makeLenses ''Class
makeLenses ''LevelClass
makeLenses ''Race

instance Eq Class where
  a == b = _className a == _className b &&
          _hitDie a    == _hitDie b

instance Num Skills where
  a + b = (acrobatics +~ (b ^. acrobatics)) a

instance Show Race where
  show x = show $ x ^. raceName

instance Show Feat where
  show x = show $ x ^. featName

instance Show Class where
  show x = show $ _className x

instance Show LevelClass where
  show x = "Level HP: " <> (show $ x ^. levelHp) <> " " <>
           "Level Stats: " <> (show $ x ^. levelStats) <> " " <>
           "Level Skills: " <> (show $ x ^. levelSkills) <> " " <>
           "Level Class" <> (show $ x ^. levelClass)

instance Show Player where
  show x = "Name: " <> (show $ x ^. name) <> " " <>
           "HP: " <> (show $ x ^. hp) <> " " <>
           "Stats: " <> (show $ x ^. stats) <> " " <>
           "Skills: " <> (show $ x ^. skills) <> " " <>
           "Feats: " <> (show $ x ^. feats)

mkStats :: Stats
mkStats = Stats 0 0 0 0 0 0

mkStatsRoll :: IO Stats
mkStatsRoll = Stats <$> mkStat <*> mkStat <*> mkStat <*> mkStat <*> mkStat <*> mkStat
  -- take sum of highest 3 of 4 D6 rolls
  where mkStat = sum <$> Prelude.drop 1 <$> sort <$> replicateM 4 (randomRIO (1, 6) :: IO Int)

mkSkills :: Skills
mkSkills = Skills 0 0 0 0 0 0 0 0

mkLevelClass :: Class -> LevelClass
mkLevelClass = LevelClass 0 0 mkSkills

mkPlayer :: Player
mkPlayer = Player { _name   = "Player"
                  , _race   = human
                  , _hp     = 4
                  , _stats  = mkStats
                  , _ac     = 0
                  , _skills = mkSkills
                  , _levels = []
                  , _feats  = [] }

modifier :: Stats -> Modifiers
modifier s = Stats { _strength = m $ s ^. strength
                   , _dexterity = m $ s ^. dexterity
                   , _constitution = m $ s ^. constitution
                   , _intelligence = m $ s ^. intelligence
                   , _wisdom = m $ s ^. wisdom
                   , _charisma = m $ s ^. charisma }
  where m x = (x - 10) `div` 2

-- applyLevel :: LevelClass -> Player -> Player
-- applyLevel level player = (hp +~ _levelHp level) .
--                           (stats +~ _levelStats level) .
--                           (skills <>~ _levelSkills level) player

applyRaceBonus :: Player -> Player
applyRaceBonus p = applyRace (_race p) p

-- TODO fix: length . filter
applyLevel :: LevelClass -> Player -> Player
applyLevel lc player = let klass = _levelClass lc
                       in applyClass klass (Prelude.length $ Prelude.filter (\c -> klass == _levelClass c) (_levels player)) player

applyLevels :: Player -> Player
applyLevels player = Prelude.foldr applyLevel player $ _levels player

applyFeats :: Player -> Player
applyFeats player = Prelude.foldr applyFeat player $ _feats player

human :: Race
human = Race "Human" id

orc :: Race
orc = Race "Orc" $ (stats . strength +~ 2) .
                   (stats . intelligence -~ 2) .
                   (stats . charisma -~ 2)

barbarian :: Class
barbarian = Class "Barbarian" 12 $ \level ->
  case level of
    1 -> (feats <>~ [alertness, combatReflexes]) .
        (stats . strength +~ 2)
    _ -> id

alertness :: Feat
alertness = Feat "Alertness" $ skills . acrobatics +~ 2

-- TODO prereqs
combatReflexes :: Feat
combatReflexes = Feat "Combat Reflexes" $ skills . acrobatics +~ 2

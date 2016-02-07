{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module DnD.Player where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.Text

import           System.Random

import           DnD.Dice

data Stats = Stats { _strength     :: Int
                   , _dexterity    :: Int
                   , _constitution :: Int
                   , _intelligence :: Int
                   , _wisdom       :: Int
                   , _charisma     :: Int }
           deriving (Show, Eq)

data Stat = Strength
          | Dexterity
          | Constitution
          | Intelligence
          | Wisdom
          | Charisma
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

data Feat = Feat { _featName   :: Text
                 , featAllowed :: Player -> Bool
                 , applyFeat   :: Player -> Player }

data LevelClass = LevelClass { _levelHp     :: Int
                             , _levelStats  :: Int
                             , _levelSkills :: Skills
                             , _levelSpells :: [Spell]
                             , _levelClass  :: Class }

-- TODO change attackModifier based on equipped weapon
data Player = Player { _name          :: Text
                     , _race          :: Race         -- ^ player race
                     , _xp            :: Int          -- ^ experience points
                     , _hp            :: Int          -- ^ total max HP
                     , _ac            :: Int          -- ^ armor class
                     , _stats         :: Stats
                     , attackModifier :: Stat         -- ^ modifier to use for attack calculations
                     , _skills        :: Skills
                     , _spells        :: [Spell]      -- ^ known spells
                     , _levels        :: [LevelClass] -- ^ levels
                     , _feats         :: [Feat] }     -- ^ feats

data Spell = Spell { _spellName  :: Text
                   , _spellLevel :: Int
                   , applySpell  :: Player -> Player -> Player }


makePrisms ''Stat

makeLenses ''Player
makeLenses ''Skills
makeLenses ''Stats
makeLenses ''Feat
makeLenses ''Class
makeLenses ''LevelClass
makeLenses ''Race


statToLens :: Functor f => Stat -> ((Int -> f Int) -> Stats -> f Stats)
statToLens Strength = strength
statToLens Dexterity = dexterity
statToLens Constitution = constitution
statToLens Intelligence = intelligence
statToLens Wisdom = wisdom
statToLens Charisma = charisma

attackBonus :: Player -> Int
attackBonus p = (modifier $ p ^. stats) ^. statToLens (attackModifier p)

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
mkLevelClass = LevelClass 0 0 mkSkills []

mkPlayer :: Player
mkPlayer = Player { _name   = "Player"
                  , _race   = human
                  , _xp     = 0
                  , _hp     = 4
                  , _ac     = 0
                  , attackModifier = Strength
                  , _stats  = mkStats
                  , _skills = mkSkills
                  , _spells = []
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
        (stats . strength +~ 2) -- not real
    _ -> id

wizard :: Class
wizard = Class "Wizard" 4 $ \level ->
  case level of
    1 -> (spells <>~ [readMagic]) .
        (feats  <>~ [scribeScroll])
    _ -> id

alertness :: Feat
alertness = Feat "Alertness" (const True) $ skills . acrobatics +~ 2

-- how to do this???
simpleWeaponProficiency :: Feat
simpleWeaponProficiency = Feat "Simple Weapon Proficiency" (const True) id

-- TODO prereqs
combatReflexes :: Feat
combatReflexes = Feat "Combat Reflexes" (const True) $ skills . acrobatics +~ 2

weaponFinesse :: Feat
weaponFinesse = Feat "Weapon Finesse" prereq id -- TODO
  where prereq p = attackBonus p > 2 -- FIXME

scribeScroll :: Feat
scribeScroll = Feat "Scribe Scroll" (const True) id

-- 1d4 + 1 * ((caster_level - 1) / 2)
-- TODO fix
magicMissile :: Spell
magicMissile = Spell "Magic Missile" 1 $ \_ -> hp -~ 1

-- TODO has no "player" effect, find out what to do!
-- TODO only a level 0 spell for wiz/sor/brd/clr/drd
readMagic :: Spell
readMagic = Spell "Read Magic" 0 $ const id

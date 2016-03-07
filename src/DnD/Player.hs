{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module DnD.Player where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Data
import           Data.List
import           Data.Monoid
import           Data.Text            hiding (filter, foldr, length)

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

emptyRace :: Race
emptyRace = Race "Race" id

data Feat = Feat { _featName   :: Text
                 , featAllowed :: Player -> Bool
                 , applyFeat   :: Player -> Player }

data LevelClass = LevelClass { _levelHp     :: Int
                             , _levelStats  :: Int
                             , _levelSkills :: Skills
                             , _levelSpells :: [Spell]
                             , _levelClass  :: Class }

data ItemType = Weapon
              | Cloak
              deriving (Show)

data Item = Item { _itemType   :: ItemType
                 , _itemName   :: Text
                 , _itemDamage :: PlayerRoller }

data Paperdoll = Paperdoll { _mainHand :: Maybe Item
                           , _offHand  :: Maybe Item
                           , _back     :: Maybe Item }

-- TODO change attackModifier based on equipped weapon
data Player = Player { _name          :: Text
                     , _race          :: Race         -- ^ player race
                     , _xp            :: Int          -- ^ experience points
                     , _hp            :: Int          -- ^ total max HP
                     , _ac            :: Int          -- ^ armor class
                     , _initiative    :: Int          -- ^ base initiative
                     , _stats         :: Stats        -- ^ ability scores
                     , attackModifier :: Stat         -- ^ ability modifier to use for attack calculations
                     , _skills        :: Skills       -- ^ skills
                     , _equipped      :: Paperdoll    -- ^ all equipped items
                     , _spells        :: [Spell]      -- ^ known spells
                     , _levels        :: [LevelClass] -- ^ levels
                     , _feats         :: [Feat] }     -- ^ feats

type PlayerRollerT = ReaderT Player (Free Roller)
type PlayerRoller  = PlayerRollerT Int

runPlayerRoller :: Player -> PlayerRoller -> Int
runPlayerRoller p r = runRollerPure (mkStdGen 0) $ runReaderT r p

-- this instance of show isn't very correct
showPlayerRoller :: Player -> PlayerRoller -> String
showPlayerRoller p r = show $ runReaderT r p

-- instance Show (ReaderT Player (Free Roller) Int) where
--   show r = show $ runReaderT r $ Player { _name = "Null"
--                                         , _race = emptyRace
--                                         , _xp   = 0
--                                         , _hp   = 0
--                                         , _ac   = 0
--                                         , _initiative = 0
--                                         , _stats = Stats 0 0 0 0 0 0
--                                         , attackModifier = Strength
--                                         , _skills = Skills 0 0 0 0 0 0 0 0
--                                         , _spells = []
--                                         , _levels = []
--                                         , _feats  = [] }

data SpellEffect = Target
                 | MultiTarget PlayerRoller -- ^ Player(caster) -> Int(number of targets)
                 | Area
                 | Self
                 | Aura

data SpellSchool = Abjuration
                 | Conjuration
                 | Divination
                 | Enchantment
                 | Evocation
                 | Illusion
                 | Necromancy
                 | Transmutation
                 deriving (Show)

data Spell = Spell { _spellName   :: Text
                   , _spellSchool :: SpellSchool
                   , _spellLevel  :: Int
                   , _spellEffect :: SpellEffect
                   , applySpell   :: Player -> Player -> Player }


makePrisms ''Stat

makeLenses ''Player
makeLenses ''Skills
makeLenses ''Stats
makeLenses ''Feat
makeLenses ''Class
makeLenses ''LevelClass
makeLenses ''Race
makeLenses ''Spell
makeLenses ''Item

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
  a - b = (acrobatics -~ (b ^. acrobatics)) a

instance Show Race where
  show x = show $ x ^. raceName

instance Show Feat where
  show x = show $ x ^. featName

instance Show Class where
  show x = show $ _className x

instance Show LevelClass where
  show x = -- "Level HP: " <> show (x ^. levelHp) <> " " <>
           -- "Level Stats: " <> show (x ^. levelStats) <> " " <>
           -- "Level Skills: " <> show (x ^. levelSkills) <> " " <>
           "Level Class: " <> show (x ^. levelClass)

instance Show Player where
  show x = show (_name x) <> " " <>
           show (_hp x) <> " " <>
           show (_stats x) <> " " <>
           show (_skills x) <> " " <>
           showPaperdoll x (_equipped x) <> " " <>
           show (_feats x) <> " " <>
           show (_levels x) <> " " <>
           "[" <> foldr (<>) "" (fmap (\z -> showSpell x z <> ", ") (_spells x)) <> "]"

showPaperdoll player p = "Main Hand: " <> showItem player (_mainHand p) <> " " <>
                         "Off Hand: "  <> showItem player (_offHand p) <> " " <>
                         "Back: "      <> showItem player (_back p)

-- instance Show SpellEffect where
--   show x = case x of
--              Target -> "Target"
--              MultiTarget _ -> "MultiTarget"
--              Area -> "Area"
--              Self -> "Self"
--              Aura -> "Aura"

showItem :: Player -> Maybe Item -> String
showItem p Nothing  = "Nothing"
showItem p (Just i) = show (i ^. itemName) <> " " <>
                      show (i ^. itemType) <> " " <>
                      showPlayerRoller p (i ^. itemDamage)


showSpellEffect :: Player -> SpellEffect -> String
showSpellEffect p e = case e of
                        Target -> "Target"
                        MultiTarget r -> "MultiTarget " <> showPlayerRoller p r
                        Area -> "Area"
                        Self -> "Self"
                        Aura -> "Aura"

showSpell :: Player -> Spell -> String
showSpell p s = show (s ^. spellName) <> " " <>
                show (s ^. spellSchool) <> " " <>
                show (s ^. spellLevel) <> " " <>
                showSpellEffect p (s ^. spellEffect)

-- instance Show Spell where
--   show x = show (x ^. spellName) <> " " <>
--            show (x ^. spellSchool) <> " " <>
--            show (x ^. spellLevel) <> " " <>
--            show (x ^. spellEffect)

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

mkPaperdoll :: Paperdoll
mkPaperdoll = Paperdoll Nothing Nothing Nothing

mkPlayer :: Player
mkPlayer = Player { _name   = "Player"
                  , _race   = human
                  , _xp     = 0
                  , _hp     = 4
                  , _ac     = 0
                  , _initiative = 0
                  , attackModifier = Strength
                  , _stats  = mkStats
                  , _skills = mkSkills
                  , _spells = []
                  , _levels = []
                  , _equipped = mkPaperdoll
                  , _feats  = [] }

modifier :: Stats -> Modifiers
modifier s = Stats { _strength = m $ s ^. strength
                   , _dexterity = m $ s ^. dexterity
                   , _constitution = m $ s ^. constitution
                   , _intelligence = m $ s ^. intelligence
                   , _wisdom = m $ s ^. wisdom
                   , _charisma = m $ s ^. charisma }
  where m = abilityModifier
abilityModifier :: Int -> Int
abilityModifier x = (x - 10) `div` 2

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

applyAll :: Player -> Player
applyAll = applyFeats . applyLevels . applyRaceBonus

playerLevel :: Player -> Int
playerLevel = length . _levels

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

-- TODO prereqs and real effect
combatReflexes :: Feat
combatReflexes = Feat "Combat Reflexes" (const True) $ skills . acrobatics +~ 2

improvedInitiative :: Feat
improvedInitiative = Feat "Improved Initiative" (const True) $ initiative +~ 2

weaponFinesse :: Feat
weaponFinesse = Feat "Weapon Finesse" prereq id -- TODO
  where prereq p = attackBonus p >= 1 -- FIXME

scribeScroll :: Feat
scribeScroll = Feat "Scribe Scroll" (const True) id

-- should be caster level that can cast specific spell
casterLevel :: Player -> Int
casterLevel p = p ^. DnD.Player.levels ^.. (folded . filtered (isCaster . _levelClass)) ^. to length

isCaster :: Class -> Bool
isCaster c = c ^. className == "Wizard"

-- 1d4 + 1 * ((caster_level - 1) / 2)
-- TODO fix
magicMissile :: Spell
magicMissile = Spell "Magic Missile" Evocation 1 (MultiTarget targets) $ \_ -> hp -~ 1
  where targets = do
          x <- asks casterLevel
          plus $ min 5 $ 1 + max 1 ((x - 1) `div` 2)

-- TODO has no "player" effect, find out what to do!
-- TODO only a level 0 spell for wiz/sor/brd/clr/drd
readMagic :: Spell
readMagic = Spell "Read Magic" Divination 0 Self $ const id

dagger :: Item
dagger = Item Weapon "Dagger" $ roll 6

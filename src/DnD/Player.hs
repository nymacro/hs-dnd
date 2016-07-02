{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module DnD.Player where

import           Control.Lens         hiding (levels)
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Data
import           Data.List
import           Data.Monoid
import           Data.Text            hiding (filter, foldr, length)

import           System.Random

import           DnD.Dice
import           DnD.Game

import           Debug.Trace

-- | Attack type
data AttackType = AttackOfOpportunity -- ^ attack of opportunity
                | FullAttack          -- ^ full round action attack
                | Attack              -- ^ standard action attack
                deriving (Show, Eq)

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


-- | Ability modifiers
type Modifiers = Stats

data Class = Class { _className :: Text                    -- ^ class name
                   , _hitDie    :: Int                     -- ^ hit-die of the class (e.g. D4)
                   , applyClass :: Int -> Player -> Player } -- ^ class's properties

data Skills = Skills { _acrobatics     :: Int
                     , _animalHandling :: Int
                     , _arcana         :: Int
                     , _athletics      :: Int
                     , _deception      :: Int
                     , _history        :: Int
                     , _insight        :: Int
                     , _intimidation   :: Int }
              deriving (Show)

data Race = Race { _raceName :: Text              -- ^ race's name
                 , applyRace :: Player -> Player } -- ^ race's properties

emptyRace :: Race
emptyRace = Race "Race" id

data Feat = Feat { _featName   :: Text              -- ^ feat's name
                 , featAllowed :: Player -> Bool     -- ^ feat prerequisite
                 , applyFeat   :: Player -> Player } -- ^ feat's effect on a player

data WeaponType = Dagger
                | Sword
                | Axe
                | Kama
                deriving (Show)

data ItemType = Weapon WeaponType
              | Chest
              | Gloves
              | Boots
              | Cloak
              | Ring
              | Amulet
              | Ammunition
              deriving (Show)

data Item = Item { _itemType :: ItemType                       -- ^ item's type
                 , _itemName :: Text                           -- ^ item's name
                 , _itemUse  :: GameStateT PlayerRollerT Int } -- ^ item's effect

data LevelClass = LevelClass { _levelHp     :: Int     -- ^ HP increase from level gain
                             , _levelStats  :: Int     -- ^ Stats increased from level gain
                             , _levelSkills :: Skills  -- ^ Skills increased by level gain
                             , _levelSpells :: [Spell] -- ^ Spells learnt by level gain
                             , _levelClass  :: Class } -- ^ Class leveled

data Paperdoll = Paperdoll { _mainHand :: Maybe Item
                           , _offHand  :: Maybe Item
                           , _back     :: Maybe Item }

-- TODO change attackModifier based on equipped weapon
data Player = Player { _name           :: Text
                     , _race           :: Race         -- ^ player race
                     , _xp             :: Int          -- ^ experience points
                     , _hp             :: Int          -- ^ total max HP
                     , _ac             :: Int          -- ^ armor class
                     , _initiative     :: Int          -- ^ base initiative
                     , _stats          :: Stats        -- ^ ability scores
                     , _attackModifier :: Stat         -- ^ ability modifier to use for attack calculations
                     , _skills         :: Skills       -- ^ skills
                     , _equipped       :: Paperdoll    -- ^ all equipped items
                     , _spells         :: [Spell]      -- ^ known spells
                     , _levels         :: [LevelClass] -- ^ levels
                     , _feats          :: [Feat] }     -- ^ feats

type PlayerRollerT = ReaderT Player (Free Roller)
type PlayerRoller  = PlayerRollerT Int

runPlayerRoller :: Player -> PlayerRoller -> Int
runPlayerRoller p r = fst $ runRollerPure (mkStdGen 0) $ runReaderT r p

showPlayerRoller :: Player -> PlayerRoller -> String
showPlayerRoller p r = show $ runReaderT r p

data SpellEffect = Target
                 | MultiTarget PlayerRoller -- ^ Player(caster) -> Int(number of targets)
                 | Area
                 | Self
                 | Aura PlayerRoller

data SpellSchool = Abjuration
                 | Conjuration
                 | Divination
                 | Enchantment
                 | Evocation
                 | Illusion
                 | Necromancy
                 | Transmutation
                 deriving (Show, Eq)

data Spell = Spell { _spellName   :: Text
                   , _spellSchool :: SpellSchool
                   , _spellLevel  :: Int
                   , _spellEffect :: SpellEffect
                   , applySpell   :: Player -> Player -> GameStateT Identity Player }


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

statToLens :: Stat -> Getter Stats Int
statToLens Strength = strength
statToLens Dexterity = dexterity
statToLens Constitution = constitution
statToLens Intelligence = intelligence
statToLens Wisdom = wisdom
statToLens Charisma = charisma

-- | Get attack bonus for the specified player
attackBonus :: Player -> Int
attackBonus p = (modifier $ p ^. stats) ^. statToLens (_attackModifier p)

instance Eq Class where
  a == b = _className a == _className b &&
          _hitDie a    == _hitDie b

-- TODO find a nice way to do this...
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

-- | Show equipped items
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
                      show (i ^. itemType) <> " " -- <>
                      -- showPlayerRoller p (i ^. itemDamage)


showSpellEffect :: Player -> SpellEffect -> String
showSpellEffect p e = case e of
                        Target -> "Target"
                        MultiTarget r -> "MultiTarget " <> showPlayerRoller p r
                        Area -> "Area"
                        Self -> "Self"
                        Aura _ -> "Aura"

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

modifier :: Stats -> Modifiers
modifier s = Stats { _strength     = m $ s ^. strength
                   , _dexterity    = m $ s ^. dexterity
                   , _constitution = m $ s ^. constitution
                   , _intelligence = m $ s ^. intelligence
                   , _wisdom       = m $ s ^. wisdom
                   , _charisma     = m $ s ^. charisma }
  where m = abilityModifier
abilityModifier :: Int -> Int
abilityModifier x = (x - 10) `div` 2

modifierFor :: Getter Stats Int -> Stats -> Int
modifierFor g s = abilityModifier $ s ^. g

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


casterLevel, arcaneLevel, divineLevel :: Player -> Int

-- | Number of levels in classes which can use magic
casterLevel p = Prelude.length $ filter isCasterLevel $ p ^. levels

-- | Number of levels in classes with Arcane magic
arcaneLevel p = Prelude.length $ filter isArcaneCasterLevel $ p ^. levels

-- | Number of levels in classes with Divine magic
divineLevel p = Prelude.length $ filter isDivineCasterLevel $ p ^. levels

-- These are somewhat hardcoded and rely on specific instances of a class...
-- There is probably a better way to do this
isArcaneCaster, isDivineCaster, isCaster :: Class -> Bool
isArcaneCaster c = cn == "Wizard" || cn == "Sorcerer" || cn == "Bard"
  where cn = c ^. className

isDivineCaster c = cn == "Cleric" || cn == "Druid"
  where cn = c ^. className

isCaster c = isArcaneCaster c || isDivineCaster c

isLevel :: (Class -> Bool) -> LevelClass -> Bool
isLevel f = f . _levelClass

isCasterLevel, isArcaneCasterLevel, isDivineCasterLevel :: LevelClass -> Bool
isCasterLevel       = isLevel isCaster
isArcaneCasterLevel = isLevel isArcaneCaster
isDivineCasterLevel = isLevel isDivineCaster

castSpell :: Spell -> Player -> Player -> GameStateT Identity Player
castSpell = applySpell

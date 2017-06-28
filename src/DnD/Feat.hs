{-# LANGUAGE OverloadedStrings #-}
module DnD.Feat where

import           DnD.Player

import           Control.Lens
import           Data.Monoid
import           Data.Text    (pack)

alertness :: Feat
alertness = Feat "Alertness" (const True) $ skills . acrobatics +~ 2

-- TODO prereqs and real effect
combatReflexes :: Feat
combatReflexes = Feat "Combat Reflexes" (const True) $ skills . acrobatics +~ 2

improvedInitiative :: Feat
improvedInitiative = Feat "Improved Initiative" (const True) $ initiative +~ 2

weaponFinesse :: Feat
weaponFinesse = Feat "Weapon Finesse" prereq $ \player ->
  if applicable (_itemType <$> player ^. equipped ^. mainHand)
    then attackModifier .~ Dexterity $ player
    else player
  where prereq p     = attackBonus p >= 1
        applicable (Just (Weapon t)) = Light == (snd $ weaponCategory t)
        applicable _                 = False

scribeScroll :: Feat
scribeScroll = Feat "Scribe Scroll" (const True) id

-- | Create a Feat, which adds a proficiency for a specific weapon type
weaponProficiency :: WeaponType
                  -> Feat
weaponProficiency w = Feat (pack $ show w <> " Proficiency") (const True) $
  proficiencies <>~ [\item -> case _itemType item of
                                Weapon t -> t == w
                                _        -> False ]

simpleWeaponProficiency :: Feat
simpleWeaponProficiency = Feat "Simple Weapon Proficiency" (const True) $
  proficiencies <>~ [\item -> case _itemType item of
                                Weapon t -> case weaponCategory t of
                                              (Simple, _) -> True
                                              _           -> False
                                _        -> False ]

martialWeaponProficiency :: Feat
martialWeaponProficiency = Feat "Martial Weapon Proficiency" (const True) $
  proficiencies <>~ [\item -> case _itemType item of
                                Weapon t -> case weaponCategory t of
                                              (Martial, _) -> True
                                              _            -> False
                                _        -> False]

lightArmorProficiency :: Feat
lightArmorProficiency = Feat "Light Armor Proficiency" (const True) $
  proficiencies <>~ [\item -> case _itemType item of
                                Chest ArmorLight -> True
                                _                -> False]

mediumArmorProficiency :: Feat
mediumArmorProficiency = Feat "Medium Armor Proficiency" pre $
  proficiencies <>~ [\item -> case _itemType item of
                                Chest ArmorMedium -> True
                                _                 -> False]
  where pre = hasFeatByName "Light Armor Proficiency"

heavyArmorProficiency :: Feat
heavyArmorProficiency = Feat "Heavy Armor Proficiency" pre $
  proficiencies <>~ [\item -> case _itemType item of
                                Chest ArmorHeavy -> True
                                _                -> False]
  where pre p = hasFeatByName "Medium Armor Proficiency" p &&
                hasFeatByName "Light Armor Proficiency" p

shieldProficiency :: Feat
shieldProficiency = Feat "Shield Proficiency" (const True) $
  proficiencies <>~ [\item -> case _itemType item of
                                Weapon t -> isShield t
                                _        -> False]

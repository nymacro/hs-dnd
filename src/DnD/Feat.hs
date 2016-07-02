{-# LANGUAGE OverloadedStrings #-}
module DnD.Feat where

import           DnD.Player

import           Control.Lens

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

-- FIXME doesn't apply to all weapons
weaponFinesse :: Feat
weaponFinesse = Feat "Weapon Finesse" prereq $ attackModifier .~ Dexterity
  where prereq p = attackBonus p >= 1

scribeScroll :: Feat
scribeScroll = Feat "Scribe Scroll" (const True) id

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

weaponFinesse :: Feat
weaponFinesse = Feat "Weapon Finesse" prereq $ \player ->
  if applicable (_itemType <$> player ^. equipped ^. mainHand)
    then attackModifier .~ Dexterity $ player
    else player
  where prereq p     = attackBonus p >= 1
        -- FIXME applicable to "light" weapons
        applicable (Just (Weapon Dagger))     = True
        applicable (Just (Weapon SwordShort)) = True
        applicable _                          = False

scribeScroll :: Feat
scribeScroll = Feat "Scribe Scroll" (const True) id

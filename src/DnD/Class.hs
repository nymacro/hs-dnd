{-# LANGUAGE OverloadedStrings #-}
module DnD.Class where

import           DnD.Feat
import           DnD.Magic.Arcane
import           DnD.Player

import           Control.Lens

barbarian :: Class
barbarian = Class "Barbarian" 12 $ \level ->
  case level of
    1 -> (feats <>~ defaultFeats) .
         (stats . strength +~ 2) -- not a real feature of the class...
    _ -> id
  where defaultFeats = [ simpleWeaponProficiency
                       , martialWeaponProficiency
                       , lightArmorProficiency
                       , mediumArmorProficiency
                       , shieldProficiency ]

wizard :: Class
wizard = Class "Wizard" 4 $ \level ->
  case level of
    1 -> (spells <>~ [readMagic]) .
         (feats  <>~ defaultFeats)
    _ -> id
  where defaultFeats = [ scribeScroll
                       , weaponProficiency Club
                       , weaponProficiency Dagger
                       , weaponProficiency CrossbowHeavy
                       , weaponProficiency CrossbowLight
                       , weaponProficiency Quarterstaff ]

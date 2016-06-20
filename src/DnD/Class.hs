{-# LANGUAGE OverloadedStrings #-}
module DnD.Class where

import           DnD.Arcane
import           DnD.Player

import           Control.Lens

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

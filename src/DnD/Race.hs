{-# LANGUAGE OverloadedStrings #-}
module DnD.Race where

import           DnD.Player

import           Control.Lens

human :: Race
human = Race "Human" id

orc :: Race
orc = Race "Orc" $ (stats . strength +~ 2) .
                   (stats . intelligence -~ 2) .
                   (stats . charisma -~ 2)

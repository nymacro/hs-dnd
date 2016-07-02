module DnD.Item where

import           DnD.Game
import           DnD.Player

dagger :: Item
dagger = Item (Weapon Dagger) "Dagger" $ roll 6

-- hideArmor :: Item
-- hideArmor = Item Chest "Hide Armor" $ ac +~ 3

{-# LANGUAGE OverloadedStrings #-}
module DnD.Magic.Arcane where

import           DnD.Dice
import           DnD.Game
import           DnD.Player

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.List
import           Debug.Trace

-- missile damage: 1d4 + 1
-- num missiles:   (caster_level - 1) / 2
magicMissile :: Spell
magicMissile = Spell "Magic Missile" Evocation 1 (MultiTarget targets) $ \caster target -> do
    let missiles = min 5 $ 1 + (casterLevel caster - 1) `div` 2
        rolls    = roll 4 >> plus 1
    damage <- runRollerState rolls

    return $ applyDamage MagicForce damage target
  where targets = do
          x <- asks casterLevel
          plus $ min 5 $ 1 + ((x - 1) `div` 2)

burningHands :: Spell
burningHands = Spell "Burning Hands" Evocation 1 Area $ \caster target -> do
    let n = min 5 $ 1 + (casterLevel caster - 1) `div` 2
        rolls = Data.List.replicate n $ roll 4
    damage <- forM rolls runRollerState

    return $ applyDamage MagicFire (sum damage) target

-- TODO has no "player" effect, find out what to do!
-- TODO only a level 0 spell for wiz/sor/brd/clr/drd
readMagic :: Spell
readMagic = Spell "Read Magic" Divination 0 Self $ \_ target -> return target

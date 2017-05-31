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
    let missiles = traceShowId $ 1 + (casterLevel caster - 1) `div` 2
        rolls    = Data.List.replicate missiles $ roll 4
    damages <- forM rolls runRollerState

    -- this could be a lot better...
    -- return $ (target ^. effects ^. applyDamage) MagicForce (sum damages + 1) target
    return $ applyDamage MagicForce (sum damages + 1) target
  where targets = do
          x <- asks casterLevel
          plus $ min 5 $ 1 + max 1 ((x - 1) `div` 2)

-- TODO has no "player" effect, find out what to do!
-- TODO only a level 0 spell for wiz/sor/brd/clr/drd
readMagic :: Spell
readMagic = Spell "Read Magic" Divination 0 Self $ \_ target -> return target

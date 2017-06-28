module DnD.Effects where

import           DnD.Player

import           Control.Monad.Cont

immunity :: EffectApply
immunity (t, d, p) = cont $ \_ -> (t, 0, p)

doubleDamage :: EffectApply
doubleDamage (t, d, p) = cont $ \r -> r (t, d * 2, p)

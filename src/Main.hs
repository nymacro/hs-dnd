{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Graphics.UI.Gtk     (AttrOp (..))
import qualified Graphics.UI.Gtk     as Gtk

import           DnD.Player

import           Control.Monad.State
import           Data.Monoid
import           System.Random

import           Control.Lens
import           Control.Monad.Free

-- this is broken...
data Roller next = Lit Int next
                 | D Int next
                 | Plus Int next
                 | Minus Int next

instance Functor Roller where
  fmap f (Lit n next)   = Lit n (f next)
  fmap f (D n next)     = D n (f next)
  fmap f (Plus n next)  = Plus n (f next)
  fmap f (Minus n next) = Minus n (f next)

lit  n  = liftF (Lit n 0)
roll n  = liftF (D n 0)
plus n  = liftF (Plus n 0)
minus n = liftF (Minus n 0)

run :: Free Roller Int -> IO Int
run r = do
  seed <- getStdRandom random
  return $ run' (mkStdGen seed) r

run' :: StdGen -> Free Roller Int -> Int
run' g (Pure r) = r
run' g (Free (Lit n next)) = n + run' g next
run' g (Free (D n next)) =
  let (roll, gen) = randomR (1, n) g
  in roll + run' gen next
run' g (Free (Plus n next))  = n + run' g next
run' g (Free (Minus n next)) = run' g next - n


makeStatsWidget :: Stats -> IO (Gtk.HBox, [Gtk.Entry]) -- IO ()
makeStatsWidget stats = do
  box <- Gtk.hBoxNew True 10
  entries <- replicateM 6 Gtk.entryNew
  let makeEntryWithStat entry f = Gtk.entrySetText entry $ show $ f stats
  zipWithM_ makeEntryWithStat entries [ _strength
                                      , _dexterity
                                      , _intelligence
                                      , _constitution
                                      , _wisdom
                                      , _charisma ]
  mapM_ (Gtk.containerAdd box) entries
  return (box, entries)

updateStatsWidget :: Stats -> [Gtk.Entry] -> IO ()
updateStatsWidget stats entries = do
  let makeEntryWithStat entry f = Gtk.entrySetText entry $ show $ f stats
  zipWithM_ makeEntryWithStat entries [ _strength
                                      , _dexterity
                                      , _intelligence
                                      , _constitution
                                      , _wisdom
                                      , _charisma ]

main :: IO ()
main = do
  rolls <- mkStatsRoll
  print rolls
  let player = mkPlayer { _stats = rolls, _race = orc, _levels = [mkLevelClass barbarian] } -- { _feats = [alertness, combatReflexes]}
  let p = applyFeats . applyLevels . applyRaceBonus $ player
  print p
  print $ modifier (p ^. stats)

  roller <- replicateM 50 $ run $ do
    lit 10
    roll 6
    roll 6
  print roller

  -- Gtk.initGUI
  -- window <- Gtk.windowNew
  -- button <- Gtk.buttonNewWithLabel "Re-roll"

  -- vbox <- Gtk.vBoxNew True 10
  -- label <- Gtk.labelNew $ Just "Hello"

  -- Gtk.containerAdd vbox label
  -- Gtk.containerAdd vbox button

  -- (statsBox, statsWidgets) <- makeStatsWidget rolls
  -- Gtk.containerAdd vbox statsBox

  -- Gtk.set window [ Gtk.windowDefaultWidth := 200
  --                , Gtk.windowDefaultHeight := 200
  --                , Gtk.containerChild := vbox
  --                , Gtk.containerBorderWidth := 10 ]

  -- Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  -- Gtk.on button Gtk.buttonActivated $ do
  --   rolls <- mkStatsRoll
  --   updateStatsWidget rolls statsWidgets

  -- Gtk.widgetShowAll window
  -- Gtk.mainGUI

{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Graphics.UI.Gtk     (AttrOp (..))
import qualified Graphics.UI.Gtk     as Gtk

import           DnD.Dice
import           DnD.Player

import           Control.Monad.State
import           Data.Monoid
import           System.Random

import           Control.Lens

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

  roller <- replicateM 50 $ runRoller $ roll 6 >> roll 6 >> plus 10
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

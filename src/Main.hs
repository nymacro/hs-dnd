{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.UI.Gtk      (AttrOp (..))
import qualified Graphics.UI.Gtk      as Gtk

import           DnD.Arcane
import           DnD.Class
import           DnD.Dice
import           DnD.Game
import           DnD.Player

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Monoid
import           Data.Text            (Text)
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
  forM_ entries $ \entry -> Gtk.containerAdd box entry >> Gtk.set entry [ Gtk.entryEditable := False ]
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

appendToLog entry content = do
  buffer <- Gtk.get entry Gtk.textViewBuffer
  end    <- Gtk.textBufferGetEndIter buffer
  Gtk.textBufferInsert buffer end $ content <> "\n"
  Gtk.textViewScrollToIter entry end 0.0 Nothing
  return ()

main :: IO ()
main = do
  rolls  <- mkStatsRoll
  rolls2 <- mkStatsRoll
  print rolls

  let wizardBase = mkPlayer { _name = "Wiz"
                            , _stats = rolls
                            , _race = human
                            , _levels = replicate 5 (mkLevelClass wizard)
                            , _spells = [magicMissile] }
      barbBase   = mkPlayer { _name = "Barb"
                            , _stats = rolls2
                            , _race = orc
                            , _levels = replicate 4 (mkLevelClass barbarian)
                            , _spells = [] }
      wiz  = applyAll wizardBase
      barb = applyAll barbBase
  print wiz
  print barb

  gen <- newStdGen
  let (barb', gen') = runIdentity $ runGameState gen $ applySpell magicMissile wiz barb
  print barb'

  roller <- replicateM 50 $ runRoller $ roll 6 >> roll 6 >> plus 10
  print roller

  -- let x = runPlayerRoller p $ do
  --           roll 6
  --           mod <- asks (\x -> abilityModifier $ x ^. stats ^. strength)
  --           plus mod
  -- print x

  -- Gtk.initGUI
  -- window   <- Gtk.windowNew
  -- Gtk.windowSetDefaultSize window 800 600
  -- button   <- Gtk.buttonNewWithLabel ("Re-roll" :: Text)
  -- logEntry <- Gtk.textViewNew
  -- Gtk.set logEntry [ Gtk.textViewEditable := False
  --                  , Gtk.textViewWrapMode := Gtk.WrapWord ]
  -- (statsBox, statsWidgets) <- makeStatsWidget rolls

  -- vbox <- Gtk.vBoxNew True 10

  -- Gtk.containerAdd vbox button
  -- Gtk.containerAdd vbox statsBox
  -- Gtk.containerAdd vbox logEntry

  -- appendToLog logEntry $ show p

  -- Gtk.set window [ Gtk.windowDefaultWidth := 200
  --                , Gtk.windowDefaultHeight := 200
  --                , Gtk.containerChild := vbox
  --                , Gtk.containerBorderWidth := 10 ]

  -- Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  -- Gtk.on button Gtk.buttonActivated $ do
  --   rolls <- mkStatsRoll
  --   updateStatsWidget rolls statsWidgets
  --   appendToLog logEntry $ show rolls

  -- Gtk.widgetShowAll window
  -- Gtk.mainGUI

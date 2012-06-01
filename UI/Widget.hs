{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module UI.Widget (
    Widget
  , filterWidget
  , filterWidgetJust
  , modifyWidget
  , modifyWidgetM
  , finiWidget
    -- * Composite widgets
  , checkbuttonGui
  , entryInt
  ) where

import Control.Monad
import Data.Maybe
import Reactive.Banana
import Reactive.Banana.Extra

import UI.Command
import UI.TclTk
import UI.TclTk.Builder
import UI.TclTk.AST

----------------------------------------------------------------
-- Widget
----------------------------------------------------------------

data Widget t s a = Widget
  { wgtName        :: TkName     -- Name of widget
  , wgtEvent       :: Event t a  -- Event for widget
  , wgtUserInput   :: Event t () -- Occurs every time on user input
  , wgtBack        :: a -> s
  , wgtInitalState :: s          -- Initial state of widget
  , wgtActimate    :: GUI t s () -- Send data to widget
  }


modifyWidget :: (b -> a) ->(Event t a -> Event t b) -> Widget t s a -> Widget t s b
modifyWidget back modify w@(Widget{..}) 
  = w { wgtEvent = modify wgtEvent 
      , wgtBack  = wgtBack . back
      }

filterWidget :: (a -> Bool) -> Widget t s a -> Widget t s a
filterWidget predicate 
  = modifyWidget id (filterE predicate)


filterWidgetJust :: Widget t s (Maybe a) -> Widget t s a
filterWidgetJust 
  = modifyWidget Just filterJust

modifyWidgetM :: (b -> a) -> (Event t a -> Event t (Maybe b)) -> Widget t s a -> Widget t s b
modifyWidgetM back modify w@(Widget{..})
  = w { wgtEvent = filterJust $ modify wgtEvent 
      , wgtBack  = wgtBack . back
      }
    
finiWidget :: Widget t s a -> GUI t p (TkName, Event t a, Behavior t s)
finiWidget (Widget{..}) = do
  let valBhv = stepper wgtInitalState (wgtBack <$> wgtEvent)
      evt    = calm $ union (valBhv  <@  wgtUserInput)
                            (wgtBack <$> wgtEvent    )
  actimateTcl evt wgtActimate
  return (wgtName,  wgtEvent, valBhv)

mkWidget :: TkName -> a -> Event t a -> GUI t a () -> Widget t a a
mkWidget nm x0 evt gui
  = Widget { wgtName        = nm
           , wgtEvent       = evt
           , wgtUserInput   = const () <$> evt
           , wgtBack        = id
           , wgtInitalState = x0
           , wgtActimate    = gui
           }



----------------------------------------------------------------
-- Composite widgets
----------------------------------------------------------------

checkbuttonGui :: [Option p] -> [Pack] -> Bool -> GUI t p (Widget t Bool Bool)
checkbuttonGui opts packs st = do
  nm <- checkbutton opts packs
  -- Capture event
  (cmd, evt) <- addTclEvent
  let Cmd pref _ = cmd undefined
  -- Set input handler
  stmt $ Stmt [ Name    "checkbutton_event_toggle"
              , Name    pref
              , WName   nm
              , LitBool st
              ]
  -- Set widget state on event
  let call = stmt $ Lam $ \f -> [Stmt [ WName nm
                                      , Name "state"
                                      , Name $ if f then "selected" else "!selected"
                                      ]
                                ]
  return $ mkWidget nm st evt call


-- | Entry which may hold space
entryInt :: [Option p]          -- ^ Entry options
         -> [Pack]              -- ^ Packing options
         -> Int                 -- ^ Initial state
         -> GUI t p (Widget t Int Int)
entryInt opts packs n = do
  -- Widget
  nm <- entry opts packs
  -- Event
  (cmd, evt) <- addTclEvent
  let Cmd pref _ = cmd undefined
  -- Set up variables
  vCur  <- freshVar
  vBack <- freshVar
  set vCur  $ LitInt n
  set vBack $ LitInt n
  configure nm $ TextVariable vCur
  -- Bind event handler
  let call = [ Name "entry_validate_int"
             , Name pref
             , Name vCur
             , Name vBack
             ]
  bind nm "<Leave>"             $ Braces call
  bind nm "<KeyPress-Return>"   $ Braces call
  bind nm "<KeyPress-KP_Enter>" $ Braces call
  --
  let tcl = do
        set vCur  $ LamE LitInt
        set vBack $ LamE LitInt
  return $ mkWidget nm n evt tcl

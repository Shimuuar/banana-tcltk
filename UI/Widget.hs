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

data Widget t a where
  Widget :: Wgt t x a -> Widget t a


data Wgt t x a = Wgt
  { wgtName        :: TkName
  , wgtEvent       :: Event t a
  , wgtUserInput   :: Event t x
  , wgtInitalState :: x
  , wgtBack        :: a -> x
  , wgtActimate    :: GUI t x ()
  }



filterWidget :: (a -> Bool) -> Widget t a -> Widget t a
filterWidget predicate (Widget w@Wgt{..})
  = Widget w { wgtEvent = filterE predicate wgtEvent
             }

filterWidgetJust :: Widget t (Maybe a) -> Widget t a
filterWidgetJust (Widget w@Wgt{..})
  = Widget w { wgtEvent = filterJust wgtEvent
             , wgtBack  = wgtBack . Just
             }

modifyWidget :: (b -> a) -> (Event t a -> Event t b) -> Widget t a -> Widget t b
modifyWidget back modify (Widget w@Wgt{..})
  = Widget w { wgtEvent = modify wgtEvent
             , wgtBack  = wgtBack . back
             }

modifyWidgetM :: (b -> a) -> (Event t a -> Event t (Maybe b)) -> Widget t a -> Widget t b
modifyWidgetM back modify (Widget w@(Wgt{..}))
  = Widget w { wgtEvent = filterJust $ modify wgtEvent
             , wgtBack  = wgtBack . back
             }

finiWidget :: Widget t a -> GUI t p (TkName, Event t a)
finiWidget (Widget (Wgt{..})) = do
  let valBehavior = stepper wgtInitalState $ wgtBack <$> wgtEvent
      evt         = calm $ union (valBehavior <@  wgtUserInput)
                                 (wgtBack     <$> wgtEvent    )
  actimateTcl evt wgtActimate
  return (wgtName,  wgtEvent)


mkWidget :: TkName -> a -> Event t a -> GUI t a () -> Widget t a
mkWidget nm x0 evt gui
  = Widget Wgt
      { wgtName        = nm
      , wgtEvent       = evt
      , wgtUserInput   = evt
      , wgtInitalState = x0
      , wgtBack        = id
      , wgtActimate    = gui
      }



----------------------------------------------------------------
-- Composite widgets
----------------------------------------------------------------


checkbuttonGui :: [Option p] -> [Pack] -> Bool -> GUI t p (Widget t Bool)
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
         -> GUI t p (Widget t Int)
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

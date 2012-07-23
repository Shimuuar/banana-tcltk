{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module UI.Widget (
    Widget
  , EB(..)
  , toEB
  , filterWidget
  , filterWidgetJust
  , modifyWidget
  , modifyWidgetM
  , finiWidget
    -- * Composite widgets
  , checkbutton
  , entryInt
  ) where

import Reactive.Banana

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Basic
import UI.TclTk.Builder



----------------------------------------------------------------
-- Widget
----------------------------------------------------------------

-- | Widgets are as name suggest GUI widgets. In addition to being
--   widgets they have associated behaviour and events which fire when
--   their value change.
data Widget t s a = Widget
  { wgtName        :: TkName     -- Tcl/Tk name of widget
  , wgtEvent       :: Event t a  -- Event for widget
  , wgtUserInput   :: Event t () -- Occurs every time on user input
  , wgtBack        :: a -> s     -- Transform current state to base state
  , wgtInitalState :: s          -- Initial state of widget
  , wgtActimate    :: GUI t s () -- Send data to widget
  }

-- | Event and its corresponding behaviour.
data EB t a = EB (Event t a) (Behavior t a)

-- | Convert widget finalization result to the 'EB' data
toEB :: (x, Event t a, Behavior t a) -> EB t a
toEB (_,e,b) = EB e b


-- | Run widget.
finiWidget :: Widget t s a -> GUI t p (TkName, Event t a, Behavior t s)
finiWidget (Widget{..}) = do
  let valBhv = stepper wgtInitalState (wgtBack <$> wgtEvent)
      evt    = calm $ union (valBhv  <@  wgtUserInput)
                            (wgtBack <$> wgtEvent    )
  actimateTcl evt wgtActimate
  return (wgtName,  wgtEvent, valBhv)



-- | Modify/filter events produced by widget.
modifyWidget :: (b -> a)                 -- ^ Transform value of new type back
             -> (Event t a -> Event t b) -- ^ Filter and/or modify events produced by widget
             -> Widget t s a             -- ^ Old widget
             -> Widget t s b
modifyWidget back modify w@(Widget{..})
  = w { wgtEvent = modify wgtEvent
      , wgtBack  = wgtBack . back
      }

-- | Analogous to 'modifyWidget' which discards 'Nothing' events.
modifyWidgetM :: (b -> a) -> (Event t a -> Event t (Maybe b)) -> Widget t s a -> Widget t s b
modifyWidgetM back modify w@(Widget{..})
  = w { wgtEvent = filterJust $ modify wgtEvent
      , wgtBack  = wgtBack . back
      }

-- | Filter events produced by widget.
filterWidget :: (a -> Bool) -> Widget t s a -> Widget t s a
filterWidget predicate
  = modifyWidget id (filterE predicate)

-- | Another variant of filter.
filterWidgetJust :: Widget t s (Maybe a) -> Widget t s a
filterWidgetJust
  = modifyWidget Just filterJust

-- | Create widget.
mkWidget :: TkName              -- ^ Widget name
         -> a                   -- ^ Initial value
         -> Event t a           -- ^ Events produced by widget
         -> GUI t a ()          -- ^ Tcl code which could produce widget
         -> Widget t a a
mkWidget nm x0 evt gui
  = Widget { wgtName        = nm
           , wgtEvent       = evt
           , wgtUserInput   = const () <$> evt
           , wgtBack        = id
           , wgtInitalState = x0
           , wgtActimate    = gui
           }

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Functor (EB t) where
  fmap f (EB e b) = EB (fmap f e) (fmap f b)

instance Apply (EB t) (EB t) where
  EB e1 b1 <@> EB e2 b2 =
    EB (union (b1 <@> e2)
              (flip ($) <$> b2 <@> e1))
       (b1 <*> b2)

instance Apply (EB t) (Event t) where
  EB e b <@> evt =
    union
      (b <@> evt)
      (filterJust $ flip fmap <$> bhv <@> e)
    where
      bhv = stepper Nothing (Just <$> evt)



----------------------------------------------------------------
-- Composite widgets
----------------------------------------------------------------

-- | Checkbutton
checkbutton :: [Option p]       -- ^ GUI options
            -> [Pack]           -- ^ Packing options
            -> Bool             -- ^ Initial state
            -> GUI t p (Widget t Bool Bool)
checkbutton opts packs st = do
  nm <- tclCheckbutton opts packs
  -- Capture event
  (pref, evt) <- addTclEvent
  -- Set input handler
  stmt $ Stmt [ Name    "checkbutton_event_toggle"
              , Name    (getEvtPrefix pref)
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


-- | Text entry which may hold integer numbers.
entryInt :: [Option p]          -- ^ Entry options
         -> [Pack]              -- ^ Packing options
         -> Int                 -- ^ Initial state
         -> GUI t p (Widget t Int Int)
entryInt opts packs n = do
  -- Widget
  nm <- tclEntry opts packs
  -- Event
  (pref, evt) <- addTclEvent
  -- Set up variables
  vCur  <- freshVar
  vBack <- freshVar
  set vCur  $ LitInt n
  set vBack $ LitInt n
  configure nm $ TextVariable vCur
  -- Bind event handler
  let call = [ Name "entry_validate_int"
             , Name (getEvtPrefix pref)
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

{-# LANGUAGE RecordWildCards #-}
module UI.Widget (
    Widget
  , filterWidget
  , modifyWidget
  , modifyWidgetM
  , finiWidget
  -- , filterWidget
  -- , modifyWidget
    -- * Composite widgets
  , checkbuttonGui
  , entryInt
  ) where

import Control.Monad
import Reactive.Banana
import Reactive.Banana.Extra

import UI.Command
import UI.TclTk
import UI.TclTk.Builder
import UI.TclTk.AST

----------------------------------------------------------------
-- Widget
----------------------------------------------------------------

data Widget t a = Widget {
    widgetName      :: TkName     -- 
  , widgetEvent     :: Event t (Maybe a)  -- 
  -- , widgetBackEvent :: Event t (Maybe a)
  , widgetSetState  :: GUI t a () -- 
  }

-- | Only accept outputs which satisfy predicate
filterWidget :: (a -> Bool) -> Widget t a -> Widget t a
filterWidget predicate w@(Widget{..}) =
  w { widgetEvent = fmap (\e -> toMaybe predicate =<< e) widgetEvent
    }

modifyWidget :: (b -> a) -> (Event t a -> Event t b) -> Widget t a -> Widget t b
modifyWidget back modify w@(Widget{..}) =
  w { widgetEvent    = maybeEvent  modify widgetEvent
    , widgetSetState = castBuilder back   widgetSetState
    }

modifyWidgetM :: (b -> a) -> (Event t a -> Event t (Maybe b)) -> Widget t a -> Widget t b
modifyWidgetM back modify w@(Widget{..}) =
  w { widgetEvent    = fmap join $ maybeEvent  modify widgetEvent
    , widgetSetState = castBuilder back   widgetSetState
    }

finiWidget :: Widget t a -> GUI t p (TkName, Event t a)
finiWidget (Widget{..}) = do
  actimateTcl (collectJusts widgetEvent) widgetSetState
  return (widgetName, filterJust widgetEvent)


toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x
  | f x       = Just x
  | otherwise = Nothing

collectJusts :: Event t (Maybe a) -> Event t a
collectJusts 
  = filterJust . scanE acc Nothing
  where
    acc x Nothing = x
    acc _ x       = x


----------------------------------------------------------------
--
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
  return $ Widget nm (fmap Just evt) call


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
  set vBack $ LitStr ""
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
  -- Update
  stmt $ Stmt call
  --
  let tcl = do
        set vCur  $ LamE LitInt
        set vBack $ LamE LitInt
  return $ Widget nm (fmap Just evt) tcl

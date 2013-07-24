module UI.Widget (
    entryInt
  ) where

import Reactive.Banana.Frameworks (Frameworks)
import Reactive.Banana.Extra

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Basic
import UI.TclTk.Builder



----------------------------------------------------------------
-- Widget
----------------------------------------------------------------

-- | Text entry which may hold integer numbers.
entryInt :: (Frameworks t, GeomManager geom)
         => [Option p]          -- ^ Entry options
         -> geom                -- ^ Packing options
         -> GUI t p (Widget t Int)
entryInt opts packs = do
  -- Widget
  nm <- tclEntry opts packs
  -- Set event handler for user input
  vCur        <- freshVar
  vBack       <- freshVar
  (pref, evt) <- addTclEvent
  configure nm $ TextVariable vCur
  let call = [ Name "entry_validate_int"
             , Name (getEvtPrefix pref)
             , Name vCur
             , Name vBack
             ]
  bind nm "<Leave>"             $ Braces call
  bind nm "<KeyPress-Return>"   $ Braces call
  bind nm "<KeyPress-KP_Enter>" $ Braces call
  -- Done
  return $ Widget nm evt
         $ \bhv -> do actimateTclB bhv $ do
                        set vCur  $ LamE LitInt
                        set vBack $ LamE LitInt
                      return $ mixEvents evt bhv

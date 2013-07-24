module UI.Widget (
    entryInt
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

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
         -> Behavior t Int      -- ^ Initial state
         -> GUI t p (TkName,Event t Int)
entryInt opts packs bhvN = do
  -- Widget
  nm <- tclEntry opts packs
  -- Set event handler for user input
  vCur        <- freshVar
  vBack       <- freshVar
  (pref, evt) <- addTclEvent
  let call = [ Name "entry_validate_int"
             , Name (getEvtPrefix pref)
             , Name vCur
             , Name vBack
             ]
  bind nm "<Leave>"             $ Braces call
  bind nm "<KeyPress-Return>"   $ Braces call
  bind nm "<KeyPress-KP_Enter>" $ Braces call
  -- Mirror behavior on the widget
  actimateTclB bhvN $ do set vCur  $ LamE LitInt
                         set vBack $ LamE LitInt
  -- Done
  return (nm,evt)


module UI.TclTk.Dialog (
    dialogSaveFile
  , dialogOpenFile
  ) where

import Reactive.Banana

import UI.TclTk.AST
import UI.TclTk.Builder



-- | In responce to event create file dialog which may generate event
--   with given prefix
dialogSaveFile :: EvtPrefix FilePath
               -> Event t ()
               -> GUI t p ()
dialogSaveFile pref evt = actimateTcl evt $ do
  stmt $ Stmt [Name "dialog_save_file", LitStr (getEvtPrefix pref)]

-- | Same as 'dialogSaveFile' but creates open dialog
dialogOpenFile :: EvtPrefix FilePath
               -> Event t ()
               -> GUI t p ()
dialogOpenFile pref evt = actimateTcl evt $ do
  stmt $ Stmt [Name "dialog_open_file", LitStr (getEvtPrefix pref)]

module UI.TclTk.Dialog (
    dialogSaveFile
  , dialogOpenFile
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import UI.TclTk.AST
import UI.TclTk.Builder



-- | In responce to event create file dialog which may generate event
--   with given prefix
dialogSaveFile :: Frameworks t
               => EvtPrefix FilePath
               -> Event t ()
               -> [(String,String)]
               -> GUI t p ()
dialogSaveFile pref evt ftypes = actimateTcl evt $ do
  stmt $ Stmt [ Name "dialog_save_file"
              , LitStr (getEvtPrefix pref)
              , Braces
                  [ Braces [ LitStr nm, LitStr mask ] | (nm,mask) <- ftypes]
              ]

-- | Same as 'dialogSaveFile' but creates open dialog
dialogOpenFile :: Frameworks t
               => EvtPrefix FilePath
               -> Event t ()
               -> [(String,String)]
               -> GUI t p ()
dialogOpenFile pref evt ftypes = actimateTcl evt $ do
  stmt $ Stmt [ Name "dialog_open_file"
              , LitStr (getEvtPrefix pref)
              , Braces
                  [ Braces [ LitStr nm, LitStr mask ] | (nm,mask) <- ftypes]
              ]

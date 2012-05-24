module UI.Widget.Choice (
  choiceWidget
  ) where

import Control.Applicative
import Control.Monad

import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Command



choiceWidget :: [(String, Event t a -> GUI t p TkName)] -- List of choices
             -> Event t a
             -> GUI t p ()
choiceWidget [] _ = return ()
choiceWidget xs evt = do
  (cmd,idxEvt) <- addTclEvent
  let Cmd pref _ = cmd undefined
  -- notebook widget
  note   <- widget "ttk::notebook"
              [] [Fill FillX] []
  -- Bind tab change event
  stmt $ Stmt [ Name "bind"
              , WName note
              , Name "<<NotebookTabChanged>>"
              , Braces [ Name "puts"
                       , Eval [ Name "concat"
                              , LitStr pref
                              , Eval [ WName note
                                     , Name "index"
                                     , Name "current"
                                     ]
                              ]
                       ]
              ]
  -- Add child widgets
  enterWidget note $ do
    forM_ (zip [0::Int ..] xs) $ \(i,(title, gui)) -> do
      wdgt <- gui $ tabEvents i evt idxEvt
      stmt $ Stmt [ WName note
                  , Name  "add"  , WName  wdgt
                  , Name  "-text", LitStr title
                  ]



data TabEvt a
  = Evt a
  | Tab Int

tabEvents :: Int -> Event t a -> Event t Int -> Event t a
tabEvents n evt tabs
  = filterJust
  $ fmap select
  $ scanE go (Nothing,Nothing)
  $ (Evt <$> evt)`union` (Tab <$> tabs)
  where
    go (_,i) (Evt a) = (Just a, i)
    go (e,_) (Tab i) = (e, Just i)

    select (Just e, Just i)
      | i == n = Just e
    select _   = Nothing

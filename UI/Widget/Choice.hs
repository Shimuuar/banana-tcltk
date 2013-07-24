module UI.Widget.Choice (
  choiceWidget
  ) where

import Control.Applicative

import Reactive.Banana
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder


-- | Tabbed widget
choiceWidget
  :: Frameworks t
  => [(String, Event t a -> GUI t p TkName)]
  -- ^ List of pairs. Tab name and tab content parameterized by
  -- event. Event is passed to the tab content when tab is
  -- switched. When new event arrives it's sent to active tab.
  -> Event t a
  -- ^ Initial event
  -> GUI t p ()
choiceWidget [] _ = return ()
choiceWidget xs evt = do
  (pref,idxEvt) <- addTclEvent
  -- notebook widget
  note <- notebook [] [Fill FillX] $
    [ (title, gui $ tabEvents i evt idxEvt)
    | (i, (title,gui)) <- zip [0::Int ..] xs 
    ]
  -- Bind tab change event
  stmt $ Stmt [ Name  "notebook_event_tab"
              , Name  (getEvtPrefix pref)
              , WName note
              ]



-- Sum of events. 
data TabEvt a
  = Evt a                       -- Event
  | Tab Int                     -- Tab change events.

-- Fiter events which apply only for current tab
tabEvents :: Int -> Event t a -> Event t Int -> Event t a
tabEvents n evt tabs
  = filterJust
  $ fmap select
  $ scanE go (Nothing, Just 0)
  $ (Evt <$> evt) `union` (Tab <$> tabs)
  where
    go (_,i) (Evt a) = (Just a, i)
    go (e,_) (Tab i) = (e, Just i)

    select (Just e, Just i)
      | i == n = Just e
    select _   = Nothing

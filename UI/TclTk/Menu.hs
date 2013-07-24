module UI.TclTk.Menu (
    -- * Data types
    Menu(..)
  , Submenu(..)
    -- * Functions
  , mainMenu
  , addSubmenu
  , menuItem
  , menuItemPress
  ) where

import Control.Monad

import Reactive.Banana
import Reactive.Banana.Frameworks

import UI.Command
import UI.TclTk
import UI.TclTk.Builder
import UI.TclTk.AST

----------------------------------------------------------------
-- Menu bar
----------------------------------------------------------------

-- | Name of menu bar
newtype Menu    = Menu    TkName

-- | Name of item on the menu bar
newtype Submenu = Submenu TkName



-- | Add main menu to the root window
mainMenu :: GUI t p Menu
mainMenu = do
  nm <- (TkName . (:[])) `liftM` freshVar
  stmt $ Stmt [ Name "menu"
              , WName nm
              ]
  stmt $ Stmt [ Name "."
              , Name "configure"
              , Name "-menu"
              , WName nm
              ]
  return $ Menu nm


-- | Add menu to the menu bar
addSubmenu :: String -> Menu -> GUI t p Submenu
addSubmenu title (Menu nm) = enterWidget nm $ do
  sub <- freshTkName
  -- Add submenu
  stmt $ Stmt [ Name "menu"
              , WName sub
              , Name "-tearoff"
              , LitInt 0
              ]
  stmt $ Stmt [ WName nm
              , Name "add"    , Name  "cascade"
              , Name "-label" , LitStr title
              , Name "-menu"  , WName  sub
              ]
  return $ Submenu sub


-- | Add item to the menu
menuItem :: (Frameworks t, Command a) => Submenu -> String -> Cmd a -> GUI t p ()
menuItem (Submenu menu) nm evt = do
  stmt $ Stmt  [ WName menu
               , Name "add" , Name "command"
               , Name "-label" , LitStr nm
               , Name "-command" , Braces $ commandExpr evt
               ]


-- | Add item to the menu which generate '()' events when pressed
menuItemPress :: Frameworks t => Submenu -> String -> GUI t p (Event t ())
menuItemPress sm nm = do
  (pref,e) <- addTclEvent
  menuItem sm nm $ Cmd pref ()
  return e

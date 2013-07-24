-- | 
-- Basic TclTk commands which doesn't generally useful for GUI
-- building.
module UI.TclTk.Basic where

import UI.TclTk.AST
import UI.TclTk.Builder

-- | puts
puts :: Expr p -> GUI t p ()
puts e
  = stmt $ Stmt [Name "puts", e]

-- | Set variable
set :: String -> Expr p -> GUI t p ()
set nm expr = stmt $ Stmt [Name "set", Name nm, expr]

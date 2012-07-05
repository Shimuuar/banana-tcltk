-- | 
-- Basic TclTk commands which doesn't generally useful for GUI
-- building.
module UI.TclTk.Basic where

import UI.TclTk.AST
import UI.TclTk.Builder

-- | puts
puts :: Monad m => Expr p -> TclBuilderT x p m ()
puts e
  = stmt $ Stmt [Name "puts", e]

-- | Set variable
set :: Monad m => String -> Expr p -> TclBuilderT x p m ()
set nm expr = stmt $ Stmt [Name "set", Name nm, expr]

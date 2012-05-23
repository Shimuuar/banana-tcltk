module UI.Widget where

import Reactive.Banana
import UI.TclTk.AST


data Widget t a 
  = Widget TkName (Event t a)

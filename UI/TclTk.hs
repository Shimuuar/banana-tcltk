-- | Tck combinators
module UI.TclTk (
    -- * Basic tcl functions
    puts
  , set
    -- * Tk widgets
    -- ** Frame
  , frame
  , frame_
  , spacer
    -- ** Label
  , label
    -- ** Button
  , button
    -- ** Checkbutton
  , tclCheckbutton
    -- ** Entry widgets
  , tclEntry
    -- ** Text widget
  , textarea
  , textReplace
    -- ** Tk commands
  , pack
  , configure
  , disable
  , bind
    -- * Callbacks
  , commandExpr
    -- * Helpers
  , widget
    -- * FRP
  , closure
  , initEvent
  , eventChanges
  , addTclEvent
  , actimateTcl
  , actimateTclB
  , actimateIO
  ) where

import UI.Command
import UI.TclTk.AST
import UI.TclTk.Builder



----------------------------------------------------------------
-- Basic Tcl functions
----------------------------------------------------------------

-- | puts
puts :: Monad m => Expr p -> TclBuilderT x p m ()
puts e
  = stmt $ Stmt [Name "puts", e]

-- | Set variable
set :: Monad m => String -> Expr p -> TclBuilderT x p m ()
set nm expr = stmt $ Stmt [Name "set", Name nm, expr]



----------------------------------------------------------------
-- Tk widgets
----------------------------------------------------------------

-- | Tk frame widget used as container
frame :: Monad m => [Pack] -> TclBuilderT x p m a -> TclBuilderT x p m (TkName,a)
frame packs content = do
  nm <- widget "ttk::frame"
          [ Padding 10 ]
          packs
          []
  x <- enterWidget nm content
  return (nm,x)

frame_ :: Monad m => [Pack] -> TclBuilderT x p m a -> TclBuilderT x p m TkName
frame_ packs content = do
  (nm, _) <- frame packs content
  return nm

spacer :: Monad m => TclBuilderT x p m TkName
spacer = frame_ [Expand True, Fill FillBoth]
       $ return ()



-- | Tk label
label :: Monad m => [Option p] -> [Pack] -> TclBuilderT x p m TkName
label opts packs
  = widget "ttk::label" opts packs []




-- | Tk button
button :: (Monad m, Command a) => [Option p] -> [Pack] -> Cmd a -> TclBuilderT x p m TkName
button opts packs cmd
  = widget "ttk::button"
      opts
      packs
      [ Name "-command"
      , Braces $ commandExpr cmd
      ]


tclCheckbutton :: (Monad m) => [Option p] -> [Pack] -> TclBuilderT x p m TkName
tclCheckbutton opts packs
  = widget "ttk::checkbutton" opts packs []

-- | Entry widget
tclEntry :: Monad m => [Option p] -> [Pack] -> TclBuilderT x p m TkName
tclEntry opts packs
  = widget "ttk::entry" opts packs []

-- | Tk text area
textarea :: (Monad m) => [Option p] -> [Pack] -> TclBuilderT x p m TkName
textarea opts packs
  = widget "tk::text" opts packs []

textReplace :: Monad m => TkName -> Expr p -> TclBuilderT x p m ()
textReplace nm str
  = stmt $ Stmt [ WName nm
                    , Name "replace" , Name "0.0" , Name "end" , str
                    ]



----------------------------------------------------------------
-- Tk commands
----------------------------------------------------------------

-- | Pack widget using current packing if not provides
pack :: Monad m => TkName -> [Pack] -> TclBuilderT x p m ()
pack nm packs = do
  opts <- case [() | Side _ <- packs ] of
             [] -> do c <- askPacking
                      return (Side c : packs)
             _  -> return packs
  stmt $ Stmt $ [ Name "pack"
                , WName nm
                ] ++ (renderPack =<< opts)

configure :: Monad m => TkName -> Option p -> TclBuilderT x p m ()
configure nm opt
  = stmt $ Stmt $ WName nm : Name "configure" : renderOption opt

disable :: Monad m => TkName -> Bool -> TclBuilderT x p m ()
disable nm flag
  = stmt $ Stmt [ WName nm
                , Name "state"
                , Name $ if flag then "disabled" else "!disabled"
                ]

bind :: Monad m => TkName -> String -> Expr p -> TclBuilderT x p m ()
bind nm evt expr
  = stmt $ Stmt [ Name  "bind"
                , WName nm
                , Name  evt
                , expr
                ]



----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

commandExpr :: Command a => Cmd a -> [Expr p]
commandExpr (Cmd pref action) =
  [ Name "puts"
  , LitStr command
  ]
  where
    command = unwords $ pref : encode action



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Create Tk widget
widget :: Monad m
       => String                -- ^ Widget constructor
       -> [Option p]            -- ^ Options
       -> [Pack]                -- ^ Packing options
       -> [Expr p]              -- ^ Arbitrary expressions
       -> TclBuilderT x p m TkName
widget wdgt opts packs exprs = do
  nm <- freshTkName
  stmt
    $ Stmt (Name wdgt : WName nm : (renderOption =<< opts) ++ exprs)
  pack nm packs
  return nm

-- | Tck combinators
module UI.TclTk (
    -- * Basic tcl functions
    puts
  , set 
    -- * Tk widgets
  , frame
  , button
  , label
    -- ** Tk commands
  , pack
  , configure
  , actimateTcl
  ) where

import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Reactive



----------------------------------------------------------------
-- Basic Tcl functions
----------------------------------------------------------------

-- | puts 
puts :: Monad m => Expr p -> TclBuilder p m ()
puts e 
  = tellStmt $ Stmt [Name "puts", e]

-- | Set variable
set :: Monad m => String -> Expr p -> TclBuilder p m ()
set nm expr = tellStmt $ Stmt [Name "set", Name nm, expr]



----------------------------------------------------------------
-- Tk widgets
----------------------------------------------------------------

-- | Tk frame widget used as container
frame :: Monad m => [Pack] -> TclBuilder p m a -> TclBuilder p m a
frame packs content = do
  nm <- widget "ttk::frame" 
          []
          packs
          [ Name "-relief"
          , Name "groove"
          , Name "-padding", Name "10"
          ]
  enterWidget nm content

-- | Tk label
label :: Monad m => [Option p] -> [Pack] -> TclBuilder p m TkName
label opts packs
  = widget "ttk::label" opts packs []

-- | Tk button
button :: (Monad m, Command a) => [Option p] -> [Pack] -> Cmd a -> TclBuilder p m TkName
button opts packs (Cmd pref action)
  = widget "ttk::button"
      opts
      packs
      [ Name "-command"
      , Braces [Name "puts", LitStr command]
      ]
  where
    command = unwords $ pref : encode action



----------------------------------------------------------------
-- Tk commands
----------------------------------------------------------------

-- | Pack widget using current packing if not provides
pack :: Monad m => TkName -> [Pack] -> TclBuilder p m ()
pack nm packs = do
  opts <- case [() | Side _ <- packs ] of
             [] -> do c <- asks currentPack
                      return (Side c : packs)
             _  -> return packs
  tellStmt $ Stmt $ [ Name "pack"
                    , WName nm
                    ] ++ (renderPack =<< opts)

configure :: Monad m => TkName -> Option p -> TclBuilder p m ()
configure nm opt
  = tellStmt $ Stmt $ WName nm : Name "configure" : renderOption opt

actimateTcl :: Source 
            -> Event t p 
            -> TclBuilder p (NetworkDescription t) () 
            -> TclBuilder q (NetworkDescription t) ()
actimateTcl src evt command = do
  tcl <- closure command
  lift $ actimateWith (writeTclParam src tcl) evt


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Create Tk widget
widget :: Monad m 
       => String                -- ^ Widget constructor
       -> [Option p]            -- ^ Options 
       -> [Pack]                -- ^ Packing options
       -> [Expr p]              -- ^ Arbitrary expressions
       -> TclBuilder p m TkName
widget wdgt opts packs exprs = do
  nm <- freshTkName
  tellStmt 
    $ Stmt (Name wdgt : WName nm : (renderOption =<< opts) ++ exprs)
  pack nm packs
  return nm

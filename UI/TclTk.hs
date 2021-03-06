{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
-- | Tck combinators
module UI.TclTk (
    -- * GUI monad
    GUI
    -- * Geometry manager
  , GeomManager(..)
    -- * Tk widgets
    -- ** Frame
  , frame
  , frame_
    -- ** Passive widgets
  , spacer
  , label
    -- ** Active widgets
  , Widget(..)
  , wgt
  , wgtB
  , button
  , tclCheckbutton
    -- ** Textual widgets
  , tclEntry
  , textarea
  , textReplace
    -- ** Notebook widget
  , notebook
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
  , EvtPrefix(getEvtPrefix)
  , Cmd(..)
  , closure
  , guiAttached
  , addTclEvent
  , actimateTcl
  , actimateTclB
  , actimateIO
  ) where

import Control.Monad      (forM_)

import Reactive.Banana
import Reactive.Banana.Frameworks (Frameworks)
import Reactive.Banana.Extra

import UI.TclTk.AST
import UI.TclTk.Builder


----------------------------------------------------------------
-- Geometry managers
----------------------------------------------------------------

-- | Geometry managers. Tcl\/Tk provide two geometry managers: pack and grid.
--   This type class allow to choose between them.
class GeomManager geom where
  placeWidget :: TkName -> geom -> GUI t p ()

instance a ~ Pack => GeomManager [a] where
  placeWidget = pack

instance GeomManager Grid where
  placeWidget nm (Grid col row) = do
    stmt $ Stmt [ Name "grid"
                , WName nm
                , Name "-column" , LitInt col
                , Name "-row"    , LitInt row
                ]



----------------------------------------------------------------
-- Tk widgets
----------------------------------------------------------------

-- | Tk frame widget used as container.
frame :: (GeomManager geom) => geom -> GUI t p a -> GUI t p (TkName,a)
frame packs content = do
  nm <- widget "ttk::frame"
          [ Padding 5 ]
          packs
          []
  x <- enterWidget nm content
  return (nm,x)

-- | Tk frame which returns only name of frame.
frame_ :: (GeomManager geom) => geom -> GUI t p a -> GUI t p TkName
frame_ packs content = do
  (nm, _) <- frame packs content
  return nm

-- | Empty frame which takes all available space.
--
--   FIXME: How does it interact with grid ???
spacer :: GUI t p TkName
spacer = frame_ [Expand True, Fill FillBoth]
       $ return ()



-- | Tk label.
label :: (GeomManager geom) => [Option p] -> geom -> GUI t p TkName
label opts packs
  = widget "ttk::label" opts packs []


-- | Information about active widget
data Widget t a = Widget
  { widgetName    :: TkName     -- ^ Name of widget
  , widgetEvent   :: Event t a  -- ^ Events generated from user actions
  , widgetReflect :: forall p. Bhv t a -> GUI t p (Bhv t a)
    -- ^ Function to reflect some behavior on widget.
  }

-- | Reflect behaviour on widget.
wgt :: Bhv t a -> Widget t a -> GUI t p (TkName, Event t a)
wgt bhv w = do
  widgetReflect w bhv
  return (widgetName w, widgetEvent w)

wgtB :: Bhv t a -> Widget t a -> GUI t p (TkName, Bhv t a)
wgtB bhv w = do
  b <- widgetReflect w bhv
  return (widgetName w, b)


-- | Tk button which generate event when pressed.
button :: (GeomManager geom, Frameworks t)
       => [Option p] -> geom -> GUI t p (TkName, Event t ())
button opts packs = do
  nm       <- widget "ttk::button" opts packs []
  (pref,e) <- addTclEvent
  stmt $ Stmt [ WName nm
              , Name "configure"
              , Name "-command"
              , Braces $ commandExpr $ Cmd pref ()
              ]
  return (nm,e)


-- | Tk checkbutton
tclCheckbutton :: (Frameworks t, GeomManager geom)
                  => [Option p] -> geom
                  -> GUI t p (Widget t Bool)
tclCheckbutton opts packs = do
  nm       <- widget "ttk::checkbutton" opts packs []
  (pref,e) <- addTclEvent
  -- Set input handler
  stmt $ Stmt [ Name    "checkbutton_event_toggle"
              , Name    (getEvtPrefix pref)
              , WName   nm
              , LitBool False -- FIXME
              ]
  -- Set callback
  let call = stmt $ Lam $ \f -> [Stmt [ WName nm
                                      , Name "state"
                                      , Name $ if f then "selected" else "!selected"
                                      ]
                                ]
  return $ Widget nm e
         $ \bhv -> actimateTclB bhv call >> return (mixEvents e bhv)



-- | Entry widget
tclEntry :: (GeomManager geom) => [Option p] -> geom -> GUI t p TkName
tclEntry opts packs
  = widget "ttk::entry" opts packs []



-- | Tk text area
textarea :: (GeomManager geom) => [Option p] -> geom -> GUI t p TkName
textarea opts packs
  = widget "tk::text" opts packs []

-- | Replace text in the text area
textReplace :: TkName -> Expr p -> GUI t p ()
textReplace nm str
  = stmt $ Stmt [ WName nm
                    , Name "replace" , Name "0.0" , Name "end" , str
                    ]


-- | Notebook widget.
notebook :: (GeomManager geom)
         => [Option p]          -- ^ Widget options
         -> geom                -- ^ Geometry specifications
         -> [(String, GUI t p TkName)]
         -- ^ Function to generate list of widgets to insert
         -> GUI t p TkName
notebook opts packs widgets = do
  note <- widget "ttk::notebook" opts packs []
  enterWidget note $
    forM_ widgets $ \(title, mkWidget) -> do
      n <- mkWidget
      stmt $ Stmt [ WName note
                  , Name  "add",   WName n
                  , Name  "-text", LitStr title
                  ]
  return note


----------------------------------------------------------------
-- Tk commands
----------------------------------------------------------------

-- | Pack widget using current packing if it's not provided
--   explicitly.
pack :: TkName -> [Pack] -> GUI t p ()
pack nm packs = do
  opts <- case [() | Side _ <- packs ] of
             [] -> do c <- askPacking
                      return (Side c : packs)
             _  -> return packs
  stmt $ Stmt $ [ Name "pack"
                , WName nm
                ] ++ (renderPack =<< opts)

-- | Set option for widget
configure :: TkName -> Option p -> GUI t p ()
configure nm opt
  = stmt $ Stmt $ WName nm : Name "configure" : renderOption opt

-- | Disable widget
disable :: TkName               -- ^ Widget name
        -> Bool                 -- ^ @True@ - disable, @False@ - enable
        -> GUI t p ()
disable nm flag
  = stmt $ Stmt [ WName nm
                , Name "state"
                , Name $ if flag then "disabled" else "!disabled"
                ]

-- | Bind expressio to event.
bind :: TkName                  -- ^ Widget name
     -> String                  -- ^ Tcl/Tk event name
     -> Expr p                  -- ^ Callback to execute
     -> GUI t p ()
bind nm evt expr
  = stmt $ Stmt [ Name  "bind"
                , WName nm
                , Name  evt
                , expr
                ]



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Generic function for creating Tk widget.
widget :: (GeomManager geom)
       => String                -- ^ Widget constructor
       -> [Option p]            -- ^ Options
       -> geom                  -- ^ Packing options
       -> [Expr p]              -- ^ Arbitrary expressions
       -> GUI t p TkName
widget wdgt opts packs exprs = do
  nm <- freshTkName
  stmt
    $ Stmt (Name wdgt : WName nm : (renderOption =<< opts) ++ exprs)
  placeWidget nm packs
  return nm

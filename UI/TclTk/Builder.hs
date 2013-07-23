{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
-- | Monad transformer for building Tcl program. It provides basic
--   combinators. Most of the time you should use module 'UI.TclTk'
--   which reexport most frequently used functions and combinators.
module UI.TclTk.Builder (
    -- * Builder monad
    TclBuilderT
  , GUI
  , runTclBuilderT
  , runGUI
    -- * Basic combinators
  , castBuilder
  , stmt
    -- ** Name generation
  , freshVar
  , freshTkName
    -- ** Reader
  , enterWidget
  , withPack
  , askPacking
    -- * Events
  , EvtPrefix(getEvtPrefix)
  , Cmd(..)
  , addTclEvent
  , initEvent
  , eventChanges
  , pureEB
  , eventEB
    -- ** Actimate events
  , actimateTcl
  , actimateTclB
  , actimateIO
  , closure
  , commandExpr
  ) where

import Control.Arrow
import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Functor.Contravariant
import Data.IORef

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Extra

import UI.TclTk.AST
import UI.Command
import UI.Dispatch
import Paths_banana_tcltk



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Monad transformer for building interface. It provides fresh name
--   generation and accumulation of Tcl statements. Parameters have
--   following meaning:
--
-- [@x@] Extra parameter which could be retrieved with 'getParameter'
--       and set using 'addParameter'
--
-- [@p@] Parameter type of Tcl AST
newtype TclBuilderT t p m a
  = TclBuilderT
      (ReaderT (TclParam t)
        (WriterT [Tcl p]
           (StateT TclState m)
         )
         a
      )
  deriving ( Functor, Applicative, Monad
           , MonadWriter [Tcl p]
           )

-- | Type-restricted synonym for 'TclBuilderT' which incorporate event
--   handling.
type GUI t p a = TclBuilderT
                   t
                   p
                   (Moment t)
                   a

instance MonadTrans (TclBuilderT x p) where
  lift = TclBuilderT . lift . lift . lift

instance MonadIO m => MonadIO (TclBuilderT x p m) where
  liftIO = TclBuilderT . liftIO . liftIO . liftIO

-- State of builder
data TclState = TclState
  { counter      :: Int
  }

-- Parameters for reader
data TclParam t = TclParam
  { currentPack :: PackSide     -- Current packing order
  , tkPath      :: [String]     -- Path
  , tclDispatch :: Dispatch
  , tclInitEvt  :: Event t ()
  }



----------------------------------------------------------------
-- Run transformer
----------------------------------------------------------------

-- | Execute tcl builder
runTclBuilderT :: Monad m
               => TclBuilderT t p m a
               -> Dispatch
               -> Event t ()
               -> m (a, [Tcl p])
runTclBuilderT (TclBuilderT m) d e
  = flip evalStateT st
  $ runWriterT
  $ runReaderT m param
  where
    st = TclState
       { counter      = 0
       }
    param = TclParam
       { currentPack = PackTop
       , tkPath      = []
       , tclDispatch = d
       , tclInitEvt  = e
       }

-- | Execute GUI builder. It creates dispatch, event network and Tcl code
runGUI :: ([String] -> IO ())       -- ^ Output function
       -> (forall t. Frameworks t => GUI t () ())   -- ^ GUI
       -> IO (Dispatch, EventNetwork, [String])
runGUI out gui = do
  -- IORef for smuggling Tcl code from NetworkDescription monad
  tclRef   <- newIORef []
  -- Set up dispatch
  dispatch         <- newDispatch
  (register, push) <- newAddHandler
  setOutput   dispatch  out
  setPushInit dispatch (push ())
  -- Send library code
  -- Build NetworkDescription
  let network :: Frameworks t => Moment t ()
      network = do
        initEvt <- fromAddHandler register
        (_,tcl) <- runTclBuilderT gui dispatch initEvt
        liftIO $ writeIORef tclRef tcl
  -- Compile network
  e   <- compile network
  lib <- readFile =<< getDataFileName "tcl-bits/banana.tcl"
  ui  <- readIORef tclRef
  return (dispatch, e, lib : (renderTcl =<< ui))



----------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------

-- | Change parameter type of Tcl AST.
castBuilder :: Monad m => (q -> p) -> TclBuilderT x p m a -> TclBuilderT x q m a
castBuilder f (TclBuilderT m)
  = TclBuilderT $ mapReaderT (mapWriterT (liftM $ id *** map (contramap f))) m

-- | Add single tcl statemetn
stmt :: Monad m => Tcl p -> TclBuilderT x p m ()
stmt = tell . (:[])

-- | Generate fresh variable
freshVar :: Monad m => TclBuilderT x p m String
freshVar = uniqString "var_"

-- | Get fresh name for Tk widget. Returns (name, full name)
freshTkName :: Monad m => TclBuilderT x p m TkName
freshTkName = do
  nm   <- freshVar
  path <- liftM tkPath askParam
  return $ TkName $ path ++ [nm]



---------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Set current packing
withPack :: Monad m => PackSide -> TclBuilderT x p m a -> TclBuilderT x p m a
withPack p widget
  = withParam (\c -> c { currentPack = p }) widget

-- | Generate new Tk names for childs of given widget
enterWidget :: Monad m => TkName -> TclBuilderT x p m a -> TclBuilderT x p m a
enterWidget (TkName name) widget
  = withParam (\c -> c { tkPath = name}) widget

-- | Get current packing
askPacking :: Monad m => TclBuilderT x p m PackSide
askPacking = liftM currentPack askParam


----------------------------------------------------------------
-- Events
----------------------------------------------------------------

-- | Unique event prefix which should be used in the event callbacks
newtype EvtPrefix a = EvtPrefix { getEvtPrefix :: String }

-- | Command which could be sent back.
data Cmd a = Cmd
  { cmdPrexif :: EvtPrefix a
  , cmdValue  :: a
  }

-- | Register new event. Returns unique event prefix and event.
addTclEvent :: Frameworks t => Command a => GUI t p (EvtPrefix a, Event t a)
addTclEvent = do
  pref     <- uniqString "EVT_"
  d        <- liftM tclDispatch askParam
  register <- lift $ registerEvent d pref
  evt      <- lift $ fromAddHandler register
  return (EvtPrefix pref, evt)

-- | Generated when GUI is attached.
initEvent :: GUI t p (Event t ())
initEvent = liftM tclInitEvt askParam

-- | Changes of behavior. This function is similar to 'changes' but
--   events are generated not only when behavior changes but also when
--   GUI is attached.
eventChanges :: Frameworks t => Behavior t a -> GUI t p (Event t a)
eventChanges bhv = do
  initEvt <- initEvent
  evt     <- lift $ changes bhv
  return $ (bhv <@ initEvt) `union` evt


pureEB :: a -> GUI t p (EB t a)
pureEB x = do
  e <- initEvent
  let bhv = pure x
  return $ EB (bhv <@ e) bhv

eventEB :: a -> Event t a -> GUI t p (EB t a)
eventEB x0 evt = do
  e <- initEvent
  let bhv = stepper x0 evt
  return $ EB ((bhv <@ e) `union` evt) bhv


-- | Send Tcl commands in responce to event which changes GUI state.
--
--   IMPORTANT: command must be idempotent because event might be
--      resent if another GUI is attached to process.  Consult
--      'UI.Dispatch' for details.
actimateTcl :: Frameworks t
            => Event t p        -- ^ Event
            -> GUI t p ()       -- ^ Tcl commands
            -> GUI t q ()
actimateTcl evt command = do
  d     <- liftM tclDispatch askParam
  tcl   <- closure command
  initE <- initEvent
  lift $ actimateWith (writeTclParam d tcl)
       $ filterJust
       $ scanE2 (\s _ -> s) (\_ s  -> Just s) Nothing initE evt

-- | Mirror behavior onto GUI. Check note on 'actimateTcl'.
actimateTclB :: Frameworks t
             => Behavior t p
             -> GUI t p ()       -- ^ Tcl commands
             -> GUI t q ()
actimateTclB bhv command = do
  d     <- liftM tclDispatch askParam
  tcl   <- closure command
  evt   <- eventChanges bhv
  lift $ actimateWith (writeTclParam d tcl) evt

-- | Execute IO action in responce to event. Unlike 'actimateTcl' this
--   function ignores init events.
actimateIO :: Frameworks t
           => Event t a
           -> (a -> IO ())
           -> GUI t p ()
actimateIO evt action =
  lift $ actimateWith action evt


-- | Generate parametrized Tcl code.
closure :: Monad m => TclBuilderT x p m () -> TclBuilderT x q m [Tcl p]
closure (TclBuilderT m) =
  TclBuilderT $ do
    -- FIXME: should path be resetted???
    par   <- ask
    (_,w) <- lift $ lift $ runWriterT $ runReaderT m par
    return w

-- | Convert command to parametrized Tcl expression.
commandExpr :: Command a => Cmd a -> [Expr p]
commandExpr (Cmd (EvtPrefix pref) action) =
  [ Name "puts"
  , LitStr $ unlex $ pref : encode action
  ]



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Get state
getSt :: Monad m => TclBuilderT x p m TclState
getSt = TclBuilderT $ lift get

-- Put staye
putSt :: Monad m => TclState -> TclBuilderT x p m ()
putSt = TclBuilderT . lift . put

-- Generate unique string with given prefix
uniqString :: Monad m => String -> TclBuilderT x p m String
uniqString pref = do
  s <- getSt
  let n = counter s
  putSt  $ s { counter = n+1 }
  return $ pref ++ show n

-- Get parameter
askParam :: Monad m => TclBuilderT x p m (TclParam x)
askParam = TclBuilderT $ ask

-- withReader for TclBuilderT
withParam :: Monad m => (TclParam x -> TclParam y) -> TclBuilderT y p m a -> TclBuilderT x p m a
withParam f (TclBuilderT widget)
  = TclBuilderT $ withReaderT f widget

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
-- | Monad transfomer for building Tcl program
module UI.TclTk.Builder ( 
    -- * Builder monad
    TclBuilderT
  , GUI
  , runTclBuilderT
  , runGUI
    -- * Basic combinators
  , stmt
  , addParameter
  , getParameter
    -- ** Name generation
  , freshVar
  , freshTkName
    -- ** Reader
  , enterWidget
  , withPack
  , askPacking
    -- ** FRP bits
  , closure
  , initEvent
  , addTclEvent
  , actimateTcl
  ) where


import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.IORef

import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk.AST
import UI.Command
import UI.Dispatch


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Monad transformer for building interface.
newtype TclBuilderT x p m a
  = TclBuilderT 
      (ReaderT (TclParam x)
        (WriterT [Tcl p]
           (StateT TclState m) 
         )
         a
      )
  deriving ( Functor, Applicative, Monad
           , MonadWriter [Tcl p] 
           )

-- | Type for building FRP GUIs using Tcl/Tk
type GUI t p a = TclBuilderT 
                   (Dispatch, Event t ())
                   p
                   (NetworkDescription t)
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
data TclParam x = TclParam
  { currentPack :: PackSide     -- Current packing order
  , tkPath      :: [String]     -- Path
  , payload     :: x            -- Custom payload
  }



----------------------------------------------------------------
-- Run transformer
----------------------------------------------------------------

-- | Execute tcl builder
runTclBuilderT :: Monad m => TclBuilderT x p m a -> x -> m (a, [Tcl p])
runTclBuilderT (TclBuilderT m) x 
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
       , payload     = x
       }

runGUI :: ([String] -> IO ())       -- ^ Output function
       -> (forall t. GUI t () ())   -- ^ GUI
       -> IO (Dispatch, EventNetwork)
runGUI out gui = do
  -- IORef for smuggling Tcl code from monad
  tclRef   <- newIORef []
  -- Set up dispatch
  dispatch         <- newDispatch
  (register, push) <- newAddHandler  
  setOutput   dispatch  out
  setPushInit dispatch (push ())
  -- Network
  let network = do 
        (_,tcl) <- flip runTclBuilderT () $ do
          initEvt <- lift $ fromAddHandler register
          addParameter (dispatch, initEvt) gui
        liftIO $ writeIORef tclRef tcl
  -- Compile network
  e <- compile network
  writeTcl dispatch =<< readIORef tclRef
  return (dispatch, e)



----------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------

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

-- | Add custom parameter
addParameter :: Monad m => x -> TclBuilderT x p m a -> TclBuilderT x' p m a
addParameter p (TclBuilderT widget)
  = TclBuilderT $ withReaderT (\c -> c { payload = p }) widget

-- | Retrieve custom paramter
getParameter :: Monad m => TclBuilderT x p m x
getParameter = liftM payload askParam

-- | Get current packing
askPacking :: Monad m => TclBuilderT x p m PackSide
askPacking = liftM currentPack askParam

-- | Generate Tcl code
closure :: Monad m => TclBuilderT x p m () -> TclBuilderT x q m [Tcl p]
closure (TclBuilderT m) =
  TclBuilderT $ do
    -- FIXME: should path be resetted
    par   <- ask
    (_,w) <- lift $ lift $ runWriterT $ runReaderT m par
    return w

addTclEvent :: Command a => GUI t p (a -> Cmd a, Event t a)
addTclEvent = do
  pref     <- uniqString "EVT_"
  (d,_)    <- getParameter
  register <- lift $ registerEvent d pref
  evt      <- lift $ fromAddHandler register
  return (Cmd pref, evt)

-- | Send Tcl in responce to event
actimateTcl :: Event t p        -- ^ Event
            -> GUI t p ()       -- ^ Tcl program
            -> GUI t q ()
actimateTcl evt command = do
  (d,_) <- getParameter
  tcl   <- closure command
  lift $ actimateWith (writeTclParam d tcl) evt


initEvent :: GUI t p (Event t ())
initEvent = snd <$> getParameter



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

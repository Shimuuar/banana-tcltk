{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Monad transfomer for building Tcl program
module UI.TclTk.Builder ( 
    -- * Builder monad
    TclBuilder
  , TclParam(..)
  , runTclBuilder
    -- * Combinators
  , tellStmt
  , freshVar
  , freshTkName
  , enterWidget
  , addParameter
  , getParameter
  , withPack
  , closure
  ) where


import Control.Applicative

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class

import UI.TclTk.AST



-- | Builder monad transformer for tcl 
-- 
-- Type is 
newtype TclBuilder x p m a
  = TclBuilder 
      (ReaderT (TclParam x)
        (WriterT [Tcl p]
           (StateT TclState m) 
         )
         a
      )
  deriving ( Functor,Applicative,Monad
           , MonadWriter [Tcl p] 
           , MonadReader (TclParam x)
           )

instance MonadTrans (TclBuilder x p) where
  lift = TclBuilder . lift . lift . lift

instance MonadIO m => MonadIO (TclBuilder x p m) where
  liftIO = TclBuilder . liftIO . liftIO . liftIO

-- | Execute tcl builder
runTclBuilder :: Monad m => TclBuilder () p m () -> m [Tcl p]
runTclBuilder (TclBuilder m) = do
   (_,tcls) <- flip evalStateT st  
             $ runWriterT 
             $ runReaderT m param
   return tcls
  where
    st = TclState 
       { counter      = 0
       }
    param = TclParam
       { currentPack = PackTop
       , tkPath      = []
       , payload     = ()
       }

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
-- basic combinators
----------------------------------------------------------------

-- Get state
getSt :: Monad m => TclBuilder x p m TclState
getSt = TclBuilder $ lift get

-- Put staye  
putSt :: Monad m => TclState -> TclBuilder x p m ()
putSt = TclBuilder . lift . put

-- Generate unique string with given prefix
uniqString :: Monad m => String -> TclBuilder x p m String
uniqString pref = do
  s <- getSt
  let n = counter s
  putSt  $ s { counter = n+1 }
  return $ pref ++ show n

-- | Add single tcl statemetn
tellStmt :: Monad m => Tcl p -> TclBuilder x p m ()
tellStmt = tell . (:[])

---------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Generate fresh variable
freshVar :: Monad m => TclBuilder x p m String
freshVar = uniqString "var_"

-- | Get fresh name for Tk widget. Returns (name, full name)
freshTkName :: Monad m => TclBuilder x p m TkName
freshTkName = do
  nm   <- freshVar
  path <- asks tkPath
  return $ TkName $ path ++ [nm]


enterWidget :: Monad m => TkName -> TclBuilder x p m a -> TclBuilder x p m a
enterWidget (TkName name) (TclBuilder widget)
  = TclBuilder $ withReaderT (\c -> c { tkPath = name}) widget

-- | Set packing 
withPack :: Monad m => PackSide -> TclBuilder x p m a -> TclBuilder x p m a
withPack p (TclBuilder widget)
  = TclBuilder $ withReaderT (\c -> c { currentPack = p }) widget

addParameter :: Monad m => x -> TclBuilder x p m a -> TclBuilder x' p m a
addParameter p (TclBuilder widget)
  = TclBuilder $ withReaderT (\c -> c { payload = p }) widget

getParameter :: Monad m => TclBuilder x p m x
getParameter = asks payload

closure :: Monad m => TclBuilder x p m () -> TclBuilder x q m [Tcl p]
closure (TclBuilder m) =
  TclBuilder $ do
    par   <- ask
    (_,w) <- lift $ lift $ runWriterT $ runReaderT m par
    return w

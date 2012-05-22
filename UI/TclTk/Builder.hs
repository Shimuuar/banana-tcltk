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
newtype TclBuilder p m a
  = TclBuilder 
      (ReaderT TclParam
        (WriterT [Tcl p]
           (StateT TclState m) 
         )
         a
      )
  deriving ( Functor,Applicative,Monad
           , MonadWriter [Tcl p] 
           , MonadReader TclParam
           )

instance MonadTrans (TclBuilder p) where
  lift = TclBuilder . lift . lift . lift

instance MonadIO m => MonadIO (TclBuilder p m) where
  liftIO = TclBuilder . liftIO . liftIO . liftIO

-- | Execute tcl builder
runTclBuilder :: Monad m => TclBuilder p m () -> m [Tcl p]
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
       }

-- State of builder
data TclState = TclState 
  { counter      :: Int 
  }

-- Parameters for reader
data TclParam = TclParam
  { currentPack :: PackSide     -- Current packing order
  , tkPath      :: [String]     -- Path
  }


----------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------

-- Get state
getSt :: Monad m => TclBuilder p m TclState
getSt = TclBuilder $ lift get

-- Put staye  
putSt :: Monad m => TclState -> TclBuilder p m ()
putSt = TclBuilder . lift . put

-- Generate unique string with given prefix
uniqString :: Monad m => String -> TclBuilder p m String
uniqString pref = do
  s <- getSt
  let n = counter s
  putSt  $ s { counter = n+1 }
  return $ pref ++ show n

-- | Add single tcl statemetn
tellStmt :: Monad m => Tcl p -> TclBuilder p m ()
tellStmt = tell . (:[])

---------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Generate fresh variable
freshVar :: Monad m => TclBuilder p m String
freshVar = uniqString "var_"

-- | Get fresh name for Tk widget. Returns (name, full name)
freshTkName :: Monad m => TclBuilder p m TkName
freshTkName = do
  nm   <- freshVar
  path <- asks tkPath
  return $ TkName $ path ++ [nm]


enterWidget :: Monad m => TkName -> TclBuilder p m a -> TclBuilder p m a
enterWidget (TkName name) (TclBuilder widget)
  = TclBuilder $ withReaderT (\c -> c { tkPath = name}) widget

-- | Set packing 
withPack :: Monad m => PackSide -> TclBuilder p m a -> TclBuilder p m a
withPack p (TclBuilder widget)
  = TclBuilder $ withReaderT (\c -> c { currentPack = p }) widget

closure :: Monad m => TclBuilder p m () -> TclBuilder q m [Tcl p]
closure (TclBuilder m) =
  TclBuilder $ do
    par   <- ask
    (_,w) <- lift $ lift $ runWriterT $ runReaderT m par
    return w

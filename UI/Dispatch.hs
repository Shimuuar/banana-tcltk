module UI.Dispatch (
    Dispatch
  , newDispatch
  , setOutput
  , setPushInit
    -- * Events
  , registerEvent
  , pushInitEvent
  , pushMessage
    -- * 
  , writeTcl
  , writeTclParam
  ) where

import Control.Applicative
import Control.Monad.IO.Class

import Data.IORef
import qualified Data.Map as Map

import Reactive.Banana

import UI.TclTk.AST
import UI.Command
import UI.Log

import Text.PrettyPrint.ANSI.Leijen (red,text)


----------------------------------------------------------------
-- Dispatch
----------------------------------------------------------------

-- | Reference to the data which could be used to generate unique
--   prefixes for commands
newtype Dispatch = Dispatch (IORef DispatchT)


data DispatchT = DispatchT
  { eventDispatch :: Map.Map String ([String] -> IO ())
  , outputMessage :: [String] -> IO ()
  , pushInit      :: IO ()
  }

-- | Create new source
newDispatch :: IO Dispatch
newDispatch = do
  Dispatch <$> newIORef DispatchT { eventDispatch = Map.empty
                                  , outputMessage = const (return ())
                                  , pushInit      = return ()
                                  }

-- | Add init event
setPushInit :: Dispatch -> IO () -> IO ()
setPushInit (Dispatch s) push = do
  atomicModifyIORef s (\x -> (x { pushInit = push },()))

-- | Add output function
setOutput :: Dispatch -> ([String] -> IO ()) -> IO ()
setOutput (Dispatch s) out = do
  atomicModifyIORef s (\x -> (x { outputMessage = out}, ()))


----------------------------------------------------------------
-- Events
----------------------------------------------------------------

-- | Fire initialization event
pushInitEvent :: Dispatch -> IO ()
pushInitEvent (Dispatch s) = do
  pushInit =<< readIORef s
 
-- | Dispatch incoming event
pushMessage :: Dispatch -> [String] -> IO ()
pushMessage _ [] = return ()
pushMessage (Dispatch s) (key:msg) = do
  src <- readIORef s
  logDoc $ red $ text $ "Message: " ++ key ++ " : " ++ show msg
  case Map.lookup key $ eventDispatch src of
    Just f  -> f msg
    Nothing -> logDoc $ red $ text ">>> Unknown!"

-- |
registerEvent :: (Command a) => Dispatch -> String -> NetworkDescription t (AddHandler a)
registerEvent (Dispatch s) pref = do
  src  <- liftIO $ readIORef s
  (register, run) <- liftIO  newAddHandler
  let action str =
        case decode str of
          Just x  -> do
            -- putStrLn $ "Decoded event: " ++ show x
            run x
          Nothing -> return ()
  liftIO $ writeIORef s $ src { eventDispatch = Map.insert pref action (eventDispatch src) }
  return register
  

  
----------------------------------------------------------------
-- Output
----------------------------------------------------------------

-- | Write tcl commands
writeTcl :: Dispatch -> [Tcl ()] -> IO ()
writeTcl s tcl = writeTclParam s tcl ()

-- | Write parametrized  tcl commands to output
writeTclParam :: Dispatch -> [Tcl p] -> p -> IO ()
writeTclParam (Dispatch s) tcl p = do
  out <- outputMessage <$> readIORef s
  out $ flip renderTclParam p =<< tcl

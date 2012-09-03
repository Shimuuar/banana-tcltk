module UI.Dispatch (
    Dispatch
  , newDispatch
  , setOutput
  , setPushInit
    -- * Events
  , registerEvent
  , pushInitEvent
  , pushMessage
    -- * Write data
  , writeRenderedTcl
  , writeTcl
  , writeTclParam
  ) where

import Control.Applicative

import Data.IORef
import qualified Data.Map as Map

import Reactive.Banana
import Reactive.Banana.Frameworks

import UI.TclTk.AST
import UI.Command
import UI.Log

import Text.PrettyPrint.ANSI.Leijen (red,text)


----------------------------------------------------------------
-- Dispatch
----------------------------------------------------------------

-- | This data encapsulate sending commands to the GUI (or multiple
--   GUIs) in form of Tcl statements and dispatching events from GUI.
--   Events come in form of strings. Every line received is considered
--   to be separate event.
--
--   Serialization and deserialization of events are described in
--   'UI.Command' module. Different events are distinguished with
--   unique prefixes generated by 'registerEvent'.
--
--   Since one of design goals of library is ability to work with
--   multiple GUIs which could be added or removed at any time proper
--   initialization of newly attached GUI is required. So whenever new
--   GUI is attached init event generated. It causes resending all
--   command which alter GUI state. So every command which changes GUI
--   state must be idempotent.
--
--   Actually this data type is mutable cell which holds all relevant
--   information.
newtype Dispatch = Dispatch (IORef DispatchT)


data DispatchT = DispatchT
  { eventDispatch :: Map.Map String ([String] -> IO ())
  , outputMessage :: [String] -> IO ()
  , pushInit      :: IO ()
  }

-- | Create new dispatch. It discard every outgoing message, have
--   empty event map and null init event.
newDispatch :: IO Dispatch
newDispatch = do
  Dispatch <$> newIORef DispatchT { eventDispatch = Map.empty
                                  , outputMessage = const (return ())
                                  , pushInit      = return ()
                                  }

-- | Set init event.
setPushInit :: Dispatch -> IO () -> IO ()
setPushInit (Dispatch s) push = do
  atomicModifyIORef s (\x -> (x { pushInit = push },()))

-- | Set output function
setOutput :: Dispatch -> ([String] -> IO ()) -> IO ()
setOutput (Dispatch s) out = do
  atomicModifyIORef s (\x -> (x { outputMessage = out}, ()))


----------------------------------------------------------------
-- Events
----------------------------------------------------------------

-- | Fire initialization event
pushInitEvent :: Dispatch -> IO ()
pushInitEvent (Dispatch s) =
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


-- | Register event in the dispatch table. This function is not meant
--   to be used directly. Use 'addTclEvent' instead.
registerEvent :: (Command a, Frameworks t)
              => Dispatch       -- ^ Dispatch table
              -> String         -- ^ Unique event prefix.
              -> Moment t (AddHandler a)
registerEvent (Dispatch s) pref = do
  src  <- liftIONow $ readIORef s
  (register, run) <- liftIONow newAddHandler
  let action str =
        case decode str of
          Just x  -> run x
          Nothing -> return ()
  liftIONow $ writeIORef s $ src { eventDispatch = Map.insert pref action (eventDispatch src) }
  return register



----------------------------------------------------------------
-- Output
----------------------------------------------------------------

-- | Write raw code
writeRenderedTcl :: Dispatch -> [String] -> IO ()
writeRenderedTcl (Dispatch s) strs = do
  out <- outputMessage <$> readIORef s
  out strs

-- | Write tcl commands
writeTcl :: Dispatch -> [Tcl ()] -> IO ()
writeTcl s tcl
  = writeTclParam s tcl ()

-- | Write parametrized Tcl commands to output
writeTclParam :: Dispatch -> [Tcl p] -> p -> IO ()
writeTclParam s tcl p
  = writeRenderedTcl s $ flip renderTclParam p =<< tcl

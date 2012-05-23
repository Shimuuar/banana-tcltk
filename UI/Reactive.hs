{-# LANGUAGE Rank2Types #-}
-- |
module UI.Reactive (
    -- * Command
    Command(..)
  , TclEvent(..)
  , Cmd(..)
  , cmd
    -- * Source
  , Source
  , newSource
  , writeTcl
  , writeTclParam
  , pushMessage
  , addEventSource
    -- * UI
  , Gui
  , runTcl
  ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.IORef
import qualified Data.Map as Map

import Reactive.Banana

import System.IO
import System.Process

import UI.TclTk.AST
import UI.TclTk.Builder


----------------------------------------------------------------
-- Comamnd type class
----------------------------------------------------------------

-- | Type class for command which could be serialized and deserialized
--   strings
class Command a where
  encode :: a -> [String]
  decode :: [String] -> Maybe a

instance Command () where
  encode _      = ["()"]
  decode ["()"] = Just ()
  decode _      = Nothing

instance Command Integer where
  encode i   = [show i]
  decode [s] = case reads s of [(i,"")] -> Just i
                               _        -> Nothing
  decode _   = Nothing

instance Command Int where
  encode i   = [show i]
  decode [s] = case reads s of [(i,"")] -> Just i
                               _        -> Nothing
  decode _   = Nothing

data TclEvent t a = TclEvent
  { tclEventPrefix :: String
  , tclEvent       :: Event t a
  }

data Cmd a = Cmd
  { cmdPrexif :: String
  , cmdValue  :: a
  }

cmd :: TclEvent t a -> a -> Cmd a
cmd (TclEvent pref _) x = Cmd pref x



----------------------------------------------------------------
-- Source
----------------------------------------------------------------

-- | Reference to the data which could be used to generate unique
--   prefixes for commands
newtype Source = Source (IORef SourceT)


data SourceT = SourceT
  { eventSource   :: Map.Map String ([String] -> IO ())
  , outputMessage :: [String] -> IO ()
  , sourceCounter :: Int
  , pushInit      :: IO ()
  }

-- | Create new source
newSource :: ([String] -> IO ()) -> IO Source
newSource f = do
  s <- newIORef $ SourceT { eventSource   = Map.empty
                          , outputMessage = f
                          , sourceCounter = 0
                          , pushInit      = return ()
                          }
  return $ Source s

-- | Add init event
setPushInit :: Source -> IO () -> IO ()
setPushInit (Source s) push = do
  atomicModifyIORef s (\x -> (x { pushInit = push },()))

pushInitEvent :: Source -> IO ()
pushInitEvent (Source s) = do
  pushInit =<< readIORef s

-- | Write tcl commands
writeTcl :: Source -> [Tcl ()] -> IO ()
writeTcl s tcl = writeTclParam s tcl ()

-- | Write parametrized  tcl commands to output
writeTclParam :: Source -> [Tcl p] -> p -> IO ()
writeTclParam (Source s) tcl p = do
  out <- outputMessage <$> readIORef s
  out $ flip renderTclParam p =<< tcl

-- | Dispatch incoming event
pushMessage :: Source -> [String] -> IO ()
pushMessage _ [] = return ()
pushMessage (Source s) (key:msg) = do
  src <- readIORef s
  putStrLn $ "Message: " ++ key ++ " " ++ show msg
  case Map.lookup key $ eventSource src of
    Just f  -> f msg
    Nothing -> return ()

-- | Create event
addEventSource :: (Command a, Show a) => Source -> NetworkDescription t (TclEvent t a)
addEventSource (Source s) = do
  -- Uniq command prefix
  pref <- uniqPrefix (Source s)
  src  <- liftIO $ readIORef s
  (register, run) <- liftIO  newAddHandler
  let action str =
        case decode str of
          Just x  -> do
            -- putStrLn $ "Decoded event: " ++ show x
            run x
          Nothing -> return ()
  liftIO $ writeIORef s $ src { eventSource = Map.insert pref action (eventSource src) }
  evt <- fromAddHandler register
  return $ TclEvent pref evt

-- Generate unique command prefix
uniqPrefix :: Source -> NetworkDescription t String
uniqPrefix (Source s) = do
  x <- liftIO (readIORef s)
  liftIO $ writeIORef s x { sourceCounter = sourceCounter x + 1 }
  return $ "Uniq_" ++ show (sourceCounter x)


----------------------------------------------------------------
-- Run tcl
----------------------------------------------------------------

type Gui t p a  = TclBuilder
                    (Event t ()) -- Init event
                    p
                    (NetworkDescription t)
                    a

-- FIXME:
--  This function is incorrect
--    1. It leaves zombies around
--    2. Reader thread is not terminated
runTcl :: (forall t. Source -> Gui t () ()) -> IO ()
runTcl ui = do
  (inp,out,_,pid) <- runInteractiveProcess "wish" [] Nothing Nothing
  src <- newSource $ \hs -> do mapM_ (hPutStrLn inp) hs
                               hFlush inp
                               mapM_ putStrLn hs
  ch <- newChan
  -- Reader thread
  forkIO $ do
    let fun s = do
          -- putStrLn $ ">>> " ++ s ++ "|"
          writeChan ch s
    mapM_ fun . lines =<< hGetContents out
  -- Dispatch thread
  forkIO $ do
    forever $ pushMessage src . words =<< readChan ch
  --
  let network = do
        -- Register init event
        (register,push) <- liftIO newAddHandler
        initEvt         <- fromAddHandler register
        liftIO $ setPushInit src (push ())
        -- Build event network
        tcl <- runTclBuilder $ addParameter initEvt $ ui src
        -- Write Tcl code to child
        liftIO $ writeTcl src tcl
  ----------------------------------------------------------------
  actuate =<< compile network
  pushInitEvent src
  waitForProcess pid
  return ()

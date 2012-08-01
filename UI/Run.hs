{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module UI.Run (
    runGuiInSubprocess
  , runServerGui
  ) where

import Control.Monad
import Control.Concurrent
import Control.Exception

import Network.Socket

import Reactive.Banana

import System.IO
import System.Process

import UI.TclTk.Builder
import UI.Dispatch
import UI.Log

import Text.PrettyPrint.ANSI.Leijen


-- | Run GUI. Tcl interpreter will be executed in subprocess
runGuiInSubprocess :: (forall t. GUI t () ()) -> IO ()
runGuiInSubprocess ui
  = bracket
      (runInteractiveProcess "wish" [] Nothing Nothing)
      finalize
      run
  where
    -- Output function
    output h strs = do
        mapM_ (hPutStrLn h) strs
        hFlush h
        forM_ strs $ \s -> do
          logDoc $ green $ text s
    -- Create and execute event network
    run (inp, out, err, pid) = do
      tid                      <- myThreadId
      (dispatch, network, tcl) <- runGUI (output inp) ui
      -- Dispatch incoming messages
      forkIO $
        handle (\(e :: SomeException) -> throwTo tid e)
               (mapM_ (pushMessage dispatch . words) . lines =<< hGetContents out)
      -- Send stderr to logger
      forkIO $
        mapM_ logStr . lines =<< hGetContents err
      -- Start event loop
      writeRenderedTcl dispatch tcl
      actuate network
      pushInitEvent dispatch
      -- Wait for child process and kill it forcefully if exception is
      -- raised
      void $ waitForProcess pid
    -- Wait for subprocess
    finalize (_,_,_, pid) = do
      c <- getProcessExitCode pid
      print c
      case c of
        Nothing -> do terminateProcess pid
                      void $ forkIO $ void $ waitForProcess pid
        _       -> return ()


-- | Run GUI as server listening on the UNIX socket.
--
--   WARNING: this is experimental feature and all connections are
--            trusted.
runServerGui :: FilePath                -- ^ Path to UNIX socket
             -> (forall t. GUI t () ()) -- ^ GUI
             -> IO ()
runServerGui fname ui = do
  chIncoming <- newChan         -- Channel for events from GUI
  chOutgoing <- newChan         -- Broadcast channel for event
  -- Cunning function for writeing Tcl code.
  -- It sends message to the output channel and reads it back to
  -- prevent memory leak
  let broadcast msg = do
        writeChan chOutgoing msg
        void $ readChan chOutgoing
  -- Prepare GUI
  (dispatch, network, tcl) <- runGUI broadcast ui
  -- Set up worker thread
  _ <- forkIO $ do
         actuate network
         forever $ pushMessage dispatch . words =<< readChan chIncoming
  -- Listen on the socket
  s <- socket AF_UNIX Stream defaultProtocol
  bindSocket s $ SockAddrUnix fname
  listen s 4
  forever $ do
    (sock,_) <- accept s
    ch       <- dupChan chOutgoing
    -- Listener thread
    _ <- forkIO $ do
           let loop str = do
                 case tokenize str of
                   (evts,rest) -> do
                     mapM_ (writeChan chIncoming) evts
                     xs <- recv sock 4096
                     loop $ rest ++ xs
           loop ""
    -- Writer thread
    _ <- forkIO $ do
           sendAll sock $ unlines tcl
           pushInitEvent dispatch
           forever $
             sendAll sock . unlines =<< readChan ch
    return ()


tokenize :: String -> ([String],String)
tokenize s =
  case break (== '\n') s of
    (x, '\n':ss) -> (x:xs, rest) where (xs,rest) = tokenize ss
    (x, _      ) -> ([],x)

sendAll :: Socket -> String -> IO ()
sendAll s str = do
  n <- send s str
  case drop n str of
    [] -> return ()
    xs -> sendAll s xs

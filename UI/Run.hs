{-# LANGUAGE Rank2Types #-}
module UI.Run (
  runGuiInSubprocess
  ) where

import Control.Monad
import Control.Concurrent
import Control.Exception

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
      (dispatch, network, tcl) <- runGUI (output inp) ui
      -- Dispatch incoming messages
      forkIO $
        mapM_ (pushMessage dispatch . words) . lines =<< hGetContents out
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

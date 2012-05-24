{-# LANGUAGE Rank2Types #-}
module UI.Run (
  runGuiInSubprocess
  ) where

-- import Control.Monad
import Control.Concurrent

import Reactive.Banana

import System.IO
import System.Process

import UI.TclTk.Builder
import UI.Dispatch


-- FIXME:
--  This function is incorrect
--    1. It leaves zombies around         [Double fork]
--    2. Reader thread is not terminated  [Kill it]
runGuiInSubprocess :: (forall t. GUI t () ()) -> IO ()
runGuiInSubprocess ui = do
  -- Create child process
  (inp,out,err,pid) <- runInteractiveProcess "wish" [] Nothing Nothing
  let output hs = do 
        mapM_ (hPutStrLn inp) hs
        hFlush inp
        mapM_ putStrLn hs
  -- Generate GUI
  (dispatch, network) <- runGUI output ui
  -- Read replies from child process
  forkIO $
    mapM_ (pushMessage dispatch . words) . lines =<< hGetContents out
  -- Send stderr to stderr
  forkIO $
    hPutStr stderr =<< hGetContents err
  -- Start event loop
  actuate network
  pushInitEvent dispatch
  waitForProcess pid
  return ()

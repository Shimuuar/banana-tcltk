-- | 
-- Helpers for loggings. It ensures that strings written to stdout
-- from different threads are not interleaved with each other.
module UI.Log (
    logStr
  , logDoc
  ) where

import Control.Concurrent
import System.IO.Unsafe
import Text.PrettyPrint.ANSI.Leijen (putDoc,Doc)

-- | Write string to stdout
logStr :: String -> IO ()
logStr = withLock . putStrLn

-- | Write 'Doc' to stdout
logDoc :: Doc -> IO ()
logDoc d = withLock $ putDoc d >> putStrLn ""

withLock :: IO a -> IO ()
withLock action = do
  takeMVar lock
  action
  putMVar lock ()


lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}

module UI.Log (
    logStr
  , logDoc
  ) where

import Control.Concurrent
import System.IO.Unsafe
import Text.PrettyPrint.ANSI.Leijen (putDoc,Doc)

logStr :: String -> IO ()
logStr = withLock . putStrLn

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

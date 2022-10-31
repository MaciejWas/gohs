module Lib.Log (info, err, logError) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Lib.Errors (AppErr (AppErr), AppResult)
import System.Console.Pretty (Color (..), color)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

printer :: String -> IO ()
printer x = do
  () <- takeMVar lock
  let atomicPutStrLn str = putStrLn str >> putMVar lock ()
  atomicPutStrLn x

info :: String -> IO ()
info msg = do
  currTime <- getCurrentTime
  let stamp = show currTime
  printer $ " [ " ++ color Green stamp ++ " ]  INFO  " ++ msg

err :: String -> IO ()
err msg = do
  currTime <- getCurrentTime
  let stamp = show currTime
  printer $ " [ " ++ color Red stamp ++ " ]  INFO  " ++ msg

logError :: AppErr -> AppResult AppErr
logError apperr@(AppErr t s) = do
  liftIO (err (show t <> show s))
  return apperr

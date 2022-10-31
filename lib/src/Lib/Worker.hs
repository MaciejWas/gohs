{-# LANGUAGE OverloadedStrings #-}

module Lib.Worker (QueueWorker(..), run) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy as Lazy
import Database.Redis (Connection, runRedis)
import Lib.Errors (AppErr, AppResult, ErrType (InternalErr), orThrowIfEmpty, withMessage)
import Lib.Log (err, info)
import Lib.Queue (HasQueue (..), RedisQueue, getMsg, postMsg)
import qualified Relude

data QueueWorker a b s = QueueWorker
  { name :: String,
    listen_to :: RedisQueue a,
    publish_to :: RedisQueue b,
    loadState :: AppResult s,
    findSubQueue :: a -> Lazy.ByteString,
    ms_main :: s -> a -> AppResult b
  }

mainLoopPass :: (HasQueue a, HasQueue b) => Connection -> QueueWorker a b s -> s -> IO ()
mainLoopPass conn worker state = do
  let runApp = runRedis conn . runExceptT :: AppResult a -> IO (Either AppErr a)
  let getNewMessage = getMsg `orThrowIfEmpty` (InternalErr `withMessage` "it's ok")
  msg <- runApp getNewMessage
  case msg of
    Left err -> return ()
    Right message ->
      let performWorkerAction = ms_main worker state message
          handleNextMessage = runApp (performWorkerAction >>= postMsg) >> ignoreResult
       in forkIO handleNextMessage >> ignoreResult

run :: (HasQueue a, HasQueue b) => Connection -> QueueWorker a b s -> IO ()
run conn worker = do
  info ("Kurwa jedziemy z koksem skurwysyny: " ++ name worker)
  state <- runRedis conn (runExceptT $ loadState worker) >>= assumeSuccess
  let doTask = mainLoopPass conn worker state
  forever doTask

-- UTILS --

ignoreResult :: Monad m => m ()
ignoreResult = return ()

assumeSuccess :: Either b a -> IO a
assumeSuccess (Right x) = return x
assumeSuccess (Left _) = err "Critical error!" >> Relude.error "failed"

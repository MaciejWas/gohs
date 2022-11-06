{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib.Worker (StreamWorker (..), run) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Exception.Base (ErrorCall)
import Control.Monad (forM_, forever)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (toASCIIBytes)
import Data.UUID.V1 (nextUUID)
import qualified Database.Redis as Redis
import Lib.Errors (AppErr (AppErr), AppResult)
import Lib.Log (err, info)
import Lib.Stream (Consumer (Consumer), Group (Group), StreamMessage (..), rcv, registerConsumer, registerGroup, send)

data StreamWorker a b s = StreamWorker
  { name :: Text,
    loadState :: AppResult s,
    ms_main :: s -> a -> AppResult b
  }

mainLoopPass :: (StreamMessage a, StreamMessage b) => Redis.Connection -> Consumer a -> StreamWorker a b s -> s -> IO ()
mainLoopPass conn consumer worker state = do
  let runApp = Redis.runRedis conn . runExceptT :: AppResult a -> IO (Either AppErr a)
  msg <- runApp (rcv consumer)
  case msg of
    Left (AppErr _ errmsg) -> Lib.Log.err (show errmsg)
    Right [] -> ignoreResult
    Right messages ->
      let handleMsg msg' =
            catch
              (runApp (ms_main worker state msg' >>= send) >> ignoreResult)
              (\(e :: ErrorCall) -> Lib.Log.err ("Critical error occured: " ++ show e))
       in forM_ messages (forkIO . handleMsg)

run :: (StreamMessage msgin, StreamMessage msgout) => Redis.Connection -> StreamWorker msgin msgout state -> IO ()
run conn (worker :: StreamWorker a b s) = do
  info ("Jedziemy z koksem: " ++ show (name worker))
  (Just uuid) <- nextUUID

  let runApp = Redis.runRedis conn . runExceptT :: forall x. AppResult x -> IO (Either AppErr x)
  let group = Group (encodeUtf8 (name worker)) :: Group a
  let consoomer = Consumer group (toASCIIBytes uuid) :: Consumer a

  state <- runApp (loadState worker) >>= assumeSuccess
  info "Loaded state"

  runApp (registerGroup group)
  runApp (registerConsumer consoomer)
  info "Registered group and consumer"

  let doTask = mainLoopPass conn consoomer worker state

  info "Starting work"
  forever (doTask >> threadDelay 100000)

-- UTILS --

ignoreResult :: Monad m => m ()
ignoreResult = return ()

assumeSuccess :: Either b a -> IO a
assumeSuccess (Right x) = return x
assumeSuccess (Left _) = err "Critical error!" >> error "failed"

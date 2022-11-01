{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib.Worker (StreamWorker(..), run) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy as Lazy
import Database.Redis (Connection, runRedis)
import Lib.Errors (AppErr (AppErr), AppResult, ErrType (InternalErr), orThrowIfEmpty, withMessage, doWithRedis)
import Lib.Log (err, info)
import Lib.Queue (Stream(..), StreamMessage(..), rcv, send,registerConsumer, Consumer (Consumer), Group (Group))
import qualified Relude
import Data.Text (pack)
import qualified Data.ByteString as Eager
import qualified Data.Binary as Binary
import Relude (Text)
import Relude.String (encodeUtf8)
import Control.Monad (forM_)
import Control.Concurrent ( forkIO )
import Lib.DataModel.AuthModel (AuthRequest(AuthRequest))


data StreamWorker a b s = StreamWorker
  { name :: Text,
    loadState :: AppResult s,
    ms_main :: s -> a -> AppResult b
  }

mainLoopPass :: (StreamMessage a, StreamMessage b) => Connection -> Consumer a -> StreamWorker a b s -> s -> IO ()
mainLoopPass conn consumer worker state = do
  let runApp = runRedis conn . runExceptT :: AppResult a -> IO (Either AppErr a)
  msg <- runApp (rcv consumer)
  case msg of
    Left (AppErr _ errmsg) -> Lib.Log.err (show errmsg)
    Right [] -> ignoreResult
    Right messages ->
      let handleMsg msg = runApp (performWorkerAction msg >>= send) >> ignoreResult 
          performWorkerAction = ms_main worker state
       in forM_ messages (forkIO . handleMsg)

run :: (StreamMessage a, StreamMessage b) => Connection -> StreamWorker a b s -> IO ()
run conn (worker :: StreamWorker a b s) = do
  info ("Kurwa jedziemy z koksem skurwysyny: " ++ show (name worker))

  let runApp = runRedis conn . runExceptT :: forall x. AppResult x -> IO (Either AppErr x)
  let group = Group "gr":: Group a
  let consoomer = Consumer group "cons" :: Consumer a

  state <- runRedis conn (runExceptT $ loadState worker) >>= assumeSuccess
  registerConsumerResult <- runApp (registerConsumer consoomer)

  -- runApp (send (AuthRequest 0 0 0) )

  case registerConsumerResult of
    Left err -> Lib.Log.err $ "Failed to register consumer. " ++ show err
    Right ok -> let doTask = mainLoopPass conn consoomer worker state 
                in forever (doTask >> threadDelay 1000000)


  

-- UTILS --

ignoreResult :: Monad m => m ()
ignoreResult = return ()

assumeSuccess :: Either b a -> IO a
assumeSuccess (Right x) = return x
assumeSuccess (Left _) = err "Critical error!" >> Relude.error "failed"

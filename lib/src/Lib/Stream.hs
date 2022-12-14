{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Lib.Stream (Stream (Stream), StreamData, GroupName, ConsumerName, Group (Group), Consumer (Consumer), StreamMessage (..), registerConsumer, registerGroup, rcv, send, watch) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, decode, encode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as Eager
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Database.Redis as R
import Database.Redis.Sentinel (Reply (Integer))
import Lib.Errors
  ( AppResult,
    ErrType (InternalErr),
    doWithRedis, 
    unwrapDb,
    withMessage,
  )
import qualified Lib.Log

type GroupName = Eager.ByteString

data Group r = Group GroupName deriving (Show)

type ConsumerName = Eager.ByteString

data Consumer r = Consumer (Group r) ConsumerName deriving (Show)

data Stream r = Stream {sname :: Eager.ByteString} deriving (Show)

type StreamData = [(Eager.ByteString, Eager.ByteString)]

type Predicate r = r -> Bool

type RedisID = Eager.ByteString

defaultMsgDeserializer :: FromJSON a => StreamData -> a
defaultMsgDeserializer strdata =
  let serialized = Lazy.fromStrict $ fromJust $ lookup ("data" :: Eager.ByteString) strdata
   in fromJust (decode serialized)

defaultMsgSerializer :: (ToJSON a) => a -> StreamData
defaultMsgSerializer req = [("data", Lazy.toStrict (encode req))]

-- An object which can be put on and read from a Redis stream
class (ToJSON a, FromJSON a) => StreamMessage a where
  stream :: Stream a

  toMsg :: a -> StreamData
  toMsg = defaultMsgSerializer

  fromMsg :: StreamData -> a
  fromMsg = defaultMsgDeserializer

registerGroup :: (StreamMessage r) => Group r -> AppResult ()
registerGroup (Group gname :: Group r) =
  let (Stream strname) = stream :: Stream r
      createGroup = R.sendRequest ["XGROUP", "CREATE", strname, gname, "$", "MKSTREAM"] :: R.Redis (Either R.Reply Eager.ByteString)
   in liftIO (Lib.Log.info "Creating group...") >> doWithRedis createGroup >>= handleResult

registerConsumer :: (StreamMessage r) => Consumer r -> AppResult ()
registerConsumer (Consumer (Group gname) cname :: Consumer r) =
  let (Stream strname) = stream :: Stream r
      createConsumer = R.sendRequest ["XGROUP", "CREATECONSUMER", strname, gname, cname] :: R.Redis (Either R.Reply Eager.ByteString)
   in liftIO (Lib.Log.info "Creating consumer...") >> doWithRedis createConsumer >>= handleResult

send :: StreamMessage r => r -> AppResult ()
send (obj :: r) =
  let msg = toMsg obj
      (Stream strname) = stream :: Stream r
      streamMsgId = "*" -- autogenerated
      sendMsg = R.xadd strname streamMsgId msg
   in doWithRedis sendMsg >> ignoreResult

rcv :: StreamMessage r => Consumer r -> AppResult [r]
rcv (Consumer (Group groupName) consumerName :: Consumer r) =
  let streamReadOptions = R.XReadOpts Nothing Nothing
      (Stream strname) = stream :: Stream r
      streamsToReadFrom = [(strname, ">")]
      readOperation = R.xreadGroupOpts groupName consumerName streamsToReadFrom streamReadOptions
   in doWithRedis readOperation
        >>= unpackStreamReadResult
        >>= return . fmap snd

unpackStreamReadResult :: StreamMessage r => Either R.Reply (Maybe [R.XReadResponse]) -> AppResult [(RedisID, r)]
unpackStreamReadResult (Left err) = throwError (InternalErr `withMessage` (pack . show) err)
unpackStreamReadResult (Right Nothing) = return []
unpackStreamReadResult (Right (Just things)) = case map R.records things of
  [firstStream] -> return (deserializeAllObjs firstStream)
  firstStream : _ ->
    liftIO (Lib.Log.err "Received data from multiple streams. This should not happen")
      >> return (deserializeAllObjs firstStream)
  [] -> return []
  where
    deserializeAllObjs = fmap (\(R.StreamsRecord _id fields) -> (_id, fromMsg fields))

watch' :: StreamMessage r => RedisID -> Predicate r -> (r -> AppResult a) -> AppResult a
watch' redisId check (actOn :: r -> AppResult a) =
  let streamReadOptions = R.XReadOpts Nothing Nothing
      (Stream strname) = stream :: Stream r
      streamsToReadFrom = [(strname, redisId)]
      readOperation = R.xreadOpts streamsToReadFrom streamReadOptions
   in do
        rawmessages <- doWithRedis readOperation
        messages <- unpackStreamReadResult rawmessages :: AppResult [(RedisID, r)]
        let relevantMessages = map snd $ filter (\(_id, msg) -> check msg) messages
        let lastMsgIndex = if null messages then redisId else fst (last messages)
        case relevantMessages of
          [] ->
            liftIO (threadDelay 1000)
              >> watch' lastMsgIndex check actOn
          x : _ -> actOn x

watch :: StreamMessage r => Predicate r -> (r -> AppResult a) -> AppResult a
watch check (actOn :: r -> AppResult a) = do
  let (Stream strname) = stream :: Stream r
  lastEntry <- doWithRedis (R.xrevRange strname "+" "-" (Just 1)) >>= unwrapDb
  let redisId = case lastEntry of
        [] -> "0"
        (R.StreamsRecord redisId' _data) : _ -> redisId'
  watch' redisId check actOn

-- UTILS --

ignoreResult :: Monad m => m ()
ignoreResult = return ()

handleResult :: Show a => Either Reply a -> AppResult ()
handleResult (Left (Integer _)) = liftIO (Lib.Log.info "Ok") :: AppResult ()
handleResult (Left err) = throwError (InternalErr `withMessage` (pack . show) err) :: AppResult ()
handleResult (Right status) = liftIO (Lib.Log.info $ "Received status " ++ show status ++ " while creating group and consumer.") :: AppResult ()

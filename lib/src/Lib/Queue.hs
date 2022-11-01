{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib.Queue (Stream (Stream), StreamData, GroupName, ConsumerName, Group (Group), Consumer (Consumer), StreamMessage (..), registerConsumer, rcv, send) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary)
import qualified Data.Binary as B
import qualified Data.Binary as Binary
import qualified Data.ByteString as Eager
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe (fromJust)
import Data.Text (pack)
import Database.Redis (XReadOpts (..))
import qualified Database.Redis as R
import Lib.Errors
  ( AppResult,
    ErrType (InternalErr),
    doWithRedis,
    unwrapDb,
    withMessage,
  )
import qualified Lib.Log
import Database.Redis.Sentinel (Reply(Integer))

type GroupName = Eager.ByteString

data Group r = Group GroupName deriving (Show)

type ConsumerName = Eager.ByteString

data Consumer r = Consumer (Group r) ConsumerName deriving (Show)

data Stream r = Stream {sname :: Eager.ByteString} deriving (Show)

type StreamData = [(Eager.ByteString, Eager.ByteString)]

type Predicate r = r -> Bool

type RedisID = Eager.ByteString

defaultMsgDeserializer :: Binary a => StreamData -> a
defaultMsgDeserializer strdata =
  let serialized = fromJust $ lookup ("data" :: Eager.ByteString) strdata
   in decodeStrict serialized

defaultMsgSerializer :: (Binary a) => a -> StreamData
defaultMsgSerializer req = [("data", encodeStrict req)]

-- An object which can be put on and read from a Redis stream
class (Binary a) => StreamMessage a where
  stream :: Stream a

  toMsg :: a -> StreamData
  toMsg = defaultMsgSerializer

  fromMsg :: StreamData -> a
  fromMsg = defaultMsgDeserializer

registerConsumer :: (StreamMessage r) => Consumer r -> AppResult ()
registerConsumer consumer@(Consumer (Group gname) cname :: Consumer r) =
  let (Stream strname) = stream :: Stream r

      handleResult (Left (Integer 1)) = liftIO (Lib.Log.info "Created 1 new consumer") :: AppResult ()
      handleResult (Left err) = throwError (InternalErr `withMessage` (pack . show) err) :: AppResult ()
      handleResult (Right status) = liftIO (Lib.Log.info $ "Received status " ++ show status ++ " while creating group and consumer.") :: AppResult ()

      createGroup =    R.sendRequest ["XGROUP", "CREATE", strname, gname, "$", "MKSTREAM"] :: R.Redis (Either R.Reply Eager.ByteString)
      createConsumer = R.sendRequest ["XGROUP", "CREATECONSUMER", strname, gname, cname] :: R.Redis (Either R.Reply Eager.ByteString)
   in    liftIO (Lib.Log.info $ "Registering " ++ show consumer) 
      >> liftIO (Lib.Log.info "Creating group..." ) >> doWithRedis createGroup >>= handleResult
      >> liftIO (Lib.Log.info "Creating consumer...") >> doWithRedis createConsumer >>= handleResult


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
watch' id check (actOn :: r -> AppResult a) =
  let streamReadOptions = R.XReadOpts Nothing Nothing
      (Stream strname) = stream :: Stream r
      streamsToReadFrom = [(strname, id)]
      readOperation = R.xreadOpts streamsToReadFrom streamReadOptions
   in do
        rawmessages <- doWithRedis readOperation
        messages <- unpackStreamReadResult rawmessages :: AppResult [(RedisID, r)]
        let relevantMessages = map snd $ filter (\(id, msg) -> check msg) messages
        let lastMsgIndex = fst (last messages)
        case relevantMessages of
          [] -> watch' lastMsgIndex check actOn
          x : _ -> liftIO (Lib.Log.info "Discarded messages.") >> actOn x

watch :: StreamMessage r => Predicate r -> (r -> AppResult a) -> AppResult a
watch = watch' "0"

-- UTILS --

ignoreResult :: Monad m => m ()
ignoreResult = return ()

decodeStrict :: (Binary a) => Eager.ByteString -> a
decodeStrict = Binary.decode . Lazy.fromStrict

encodeStrict :: (Binary a) => a -> Eager.ByteString
encodeStrict = Lazy.toStrict . Binary.encode

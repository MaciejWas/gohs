{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Lib.Queue (Stream(Stream), GroupName, ConsumerName, Group, Consumer ) where

import Data.Binary (Binary)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Eager
import qualified Database.Redis as R
import Control.Monad (void)
import Lib.Errors
  ( AppResult,
    doWithRedis,
    unwrapDb, ErrType (InternalErr), withMessage,
  )
import Relude (ByteString, LazyStrict (toStrict))
import Database.Redis (XReadOpts(..))
import Data.Text (pack)
import Control.Monad.Except (throwError)

data Stream r = Stream
  {  name :: Eager.ByteString, makeId :: Maybe (r -> Eager.ByteString) }

type GroupName = Eager.ByteString
type ConsumerName = Eager.ByteString

data Group r = Group (Stream r) GroupName
data Consumer r = Consumer (Group r) ConsumerName

registerConsumer :: (StreamMessage r) => Consumer r -> AppResult ()
registerConsumer (Consumer (Group (Stream sname _) gname) name) = let handleFailure = \case {Left err -> throwError (InternalErr `withMessage` (pack . show) err); Right status -> return () }
                                            in doWithRedis (R.xgroupCreate sname gname name) >>= handleFailure >> return ()

type StreamData = [(Eager.ByteString, Eager.ByteString)]

class Binary a => StreamMessage a where
  toMsg :: a -> StreamData
  fromMsg :: StreamData -> a

send :: StreamMessage r => Stream r -> r -> AppResult ()
send stream obj = let msg = toMsg obj
                      id = case makeId stream of { Nothing -> "*"; Just f -> f obj  }
                      sendMsg = R.xadd (name stream) id msg
                  in void (doWithRedis sendMsg)

rcv :: StreamMessage r => Stream r -> AppResult (Maybe r)
rcv stream = let opts = R.XReadOpts {block=Nothing, recordCount=Just 1}
                 rcvMsg = R.xreadOpts [(name stream, "$")] opts
              in doWithRedis rcvMsg

unredis :: StreamMessage r => Maybe (Maybe [R.XReadResponse]) -> Maybe r
unredis Nothing = Nothing
unredis (Just Nothing) = Nothing
unredis (Just (Just [])) = Nothing


-- rcv :: Rcvable a => TypeHolder a -> AppResult (Maybe a)
-- rcv t =
--   let deserializeIfPresent = return . fmap deserialize
--       (RedisQueue addr)    = queueFrom t
--       getNewestMsg         = ( doWithRedis . R.lpop . toStrict ) addr
--   in getNewestMsg >>= unwrapDb >>= deserializeIfPresent

-- send :: Sendable r => AppResult (Maybe r)
-- send = getMsg' TypeHolder 

-- send' :: (Sendable r) => r -> AppResult ()
-- send' msg =
--   let serialized        = serialize msg
--       (RedisQueue addr) = queueTo msg
--       addToQueue        = doWithRedis . R.rpush (toStrict addr)
--    in addToQueue [serialized] >>= unwrapDb >> return ()

-- UTILS --

serialize :: B.Binary a => a -> ByteString
serialize = Lazy.toStrict . B.encode

deserialize :: B.Binary r => ByteString -> r
deserialize = B.decode . Lazy.fromStrict

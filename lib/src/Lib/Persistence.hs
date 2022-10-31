{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Persistence (get, randMember, getMembers, addMember, set, exists, encodeStrict, RedisAddr (..), RedisSetAddr (..), connectToRedis) where

import Control.Monad.Except (liftIO, runExceptT, throwError)
import qualified Data.Binary as B
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as Lazy
import qualified Database.Redis as R
import Lib.Errors (AppResult, dbFailedErr, doWithRedis, unwrapDb)

newtype RedisAddr r = RedisAddr Lazy.ByteString -- \| Type-safe redis interface

newtype RedisSetAddr r = RedisSetAddr Lazy.ByteString

connectToRedis :: AppResult R.Connection
connectToRedis =
  let connection = R.checkedConnect R.defaultConnectInfo
   in liftIO connection

get :: (B.Binary r, B.Binary k) => RedisAddr r -> k -> AppResult (Maybe r)
get (RedisAddr addr) key = do
  result <- (doWithRedis . R.get . toStrict) (addr <> B.encode key)
  case result of
    Right (Just response) -> return $ Just $ B.decode $ fromStrict response
    Right Nothing -> return Nothing
    Left _err -> throwError dbFailedErr

set :: (B.Binary r, B.Binary k) => RedisAddr r -> k -> r -> AppResult ()
set (RedisAddr addr) key obj =
  let serializedObj = encodeStrict obj
      fullPath = toStrict (addr <> B.encode key)
   in (doWithRedis . R.set fullPath) serializedObj >>= unwrapDb >> return ()

exists :: RedisAddr r -> Lazy.ByteString -> AppResult Bool
exists (RedisAddr addr) key =
  let fullPath = toStrict (addr <> key)
   in doWithRedis (R.exists fullPath) >>= unwrapDb

getMembers :: (B.Binary r) => RedisSetAddr r -> Lazy.ByteString -> AppResult [r]
getMembers (RedisSetAddr addr) key = do
  let fullPath = toStrict (addr <> key)
  serialized <- doWithRedis (R.smembers fullPath) >>= unwrapDb
  let deserialized = fmap (B.decode . fromStrict) serialized
  return deserialized

addMember :: (B.Binary r) => RedisSetAddr r -> Lazy.ByteString -> r -> AppResult ()
addMember addr key obj = addMembers addr key [obj]

addMembers :: (B.Binary r) => RedisSetAddr r -> Lazy.ByteString -> [r] -> AppResult ()
addMembers (RedisSetAddr addr) key xs =
  let serializedObjs = fmap encodeStrict xs
      fullPath = toStrict (addr <> key)
   in doWithRedis (R.sadd fullPath serializedObjs) >>= unwrapDb >> return ()

randMember :: (B.Binary r) => RedisSetAddr r -> Lazy.ByteString -> AppResult (Maybe r)
randMember (RedisSetAddr addr) key =
  let fullPath = toStrict (addr <> key)
      deserializeMaybe = return . fmap deserialize
   in doWithRedis (R.spop fullPath) >>= unwrapDb >>= deserializeMaybe

-- HELPERS

encodeStrict :: B.Binary a => a -> ByteString
encodeStrict = toStrict . B.encode

deserialize :: B.Binary r => ByteString -> r
deserialize = B.decode . fromStrict

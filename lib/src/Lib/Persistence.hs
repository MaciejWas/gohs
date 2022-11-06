{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Persistence (get, set, exists, encodeStrict, RedisAddr (..), RedisSetAddr (..)) where

import Control.Monad.Except (throwError)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as Lazy
import qualified Database.Redis as R
import Lib.Errors (AppResult, dbFailedErr, doWithRedis, unwrapDb)

newtype RedisAddr key val = RedisAddr Lazy.ByteString -- \| Type-safe redis interface

newtype RedisSetAddr key val = RedisSetAddr Lazy.ByteString

get :: (Binary key, Binary val) => RedisAddr key val -> key -> AppResult (Maybe val)
get (RedisAddr addr) key = do
  result <- (doWithRedis . R.get . toStrict) (addr <> Binary.encode key)
  case result of
    Right (Just response) -> return (Just (deserialize response))
    Right Nothing -> return Nothing
    Left _err -> throwError dbFailedErr

set :: (Binary key, Binary val) => RedisAddr key val -> key -> val -> AppResult ()
set (RedisAddr addr) key obj =
  let serializedObj = encodeStrict obj
      fullPath = toStrict (addr <> Binary.encode key)
   in (doWithRedis . R.set fullPath) serializedObj >>= unwrapDb >> return ()

exists :: (Binary key) => RedisAddr key val -> key -> AppResult Bool
exists (RedisAddr addr) key =
  let fullPath = toStrict (addr <> Binary.encode key)
   in doWithRedis (R.exists fullPath) >>= unwrapDb

-- HELPERS

encodeStrict :: Binary.Binary a => a -> ByteString
encodeStrict = toStrict . Binary.encode

deserialize :: Binary.Binary r => ByteString -> r
deserialize = Binary.decode . fromStrict

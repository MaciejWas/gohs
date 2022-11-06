{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.IdMap (createNewUniqueId, getNextId) where

import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Lib.Errors (AppResult)
import Lib.Persistence (RedisAddr (RedisAddr), get, set)

type ID = Int

createNewUniqueId :: Text -> RedisAddr () ID
createNewUniqueId name = RedisAddr ("id:" <> addr)
  where
    addr = Lazy.fromStrict (encodeUtf8 name) :: Lazy.ByteString

getNextId :: RedisAddr () ID -> AppResult ID
getNextId addr = do
  idx <- get addr () `or'` 0
  set addr () (idx + 1)
  return idx

or' :: AppResult (Maybe a) -> a -> AppResult a
or' appres a =
  appres
    >>= ( \case
            Just x -> return x
            Nothing -> return a
        )

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IdMap (getNextId) where

import Lib.DataModel.AuthModel (UserId (..))
import Lib.Errors (AppResult (..))
import Lib.Persistence (RedisAddr (..), get, set)

maxId :: RedisAddr UserId
maxId = RedisAddr "max:id:"

incr :: UserId -> UserId
incr (UserId id) = UserId (id + 1)

getNextId :: AppResult UserId
getNextId = do
  id <- get maxId () `or'` (UserId 0)
  set maxId () (incr id)
  return id

or' :: AppResult (Maybe a) -> a -> AppResult a
or' appres a =
  appres
    >>= ( \case
            Just x -> return x
            Nothing -> return a
        )

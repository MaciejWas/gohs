{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Api where

import Data.Binary (Binary)
import Data.ByteString.Lazy (singleton)
import qualified Data.ByteString.Lazy as Lazy
import Lib.DataModel.AuthModel (AuthRequest (AuthRequest), AuthResponse (..))
import Lib.Errors (AppResult, ErrType (AuthError))
import Lib.Queue

intToBytes :: Int -> Lazy.ByteString
intToBytes = singleton . toEnum

instance Sendable AuthRequest where
  queueTo :: AuthRequest -> RedisQueue AuthRequest
  queueTo = \_ -> RedisQueue "auth:request"

instance Sendable AuthResponse

instance Rcvable AuthResponse

instance Respondable AuthRequest AuthResponse where
  queueResp :: AuthRequest -> RedisQueue AuthResponse
  queueResp (AuthRequest cid _ _ _) = RedisQueue (intToBytes cid)
  



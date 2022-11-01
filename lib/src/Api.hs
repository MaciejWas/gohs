{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Api where

import Data.ByteString.Lazy (singleton)
import Data.ByteString as Eager ( ByteString )
import qualified Data.ByteString.Lazy as Lazy
import Lib.Queue (Stream(Stream), StreamMessage (..))
import Lib.DataModel.AuthModel ( AuthRequest, AuthResponse )
import Data.Binary ( Binary)
import qualified Data.Binary as Binary


authRequests :: Stream AuthRequest
authRequests = Stream "auth:request"

authResponse :: Stream AuthResponse
authResponse = Stream "auth:request"

-------------------------------------

instance StreamMessage AuthRequest where
  stream :: Stream AuthRequest
  stream = authRequests

instance StreamMessage AuthResponse where
  stream :: Stream AuthResponse
  stream = authResponse

decodeStrict :: (Binary a) => Eager.ByteString -> a
decodeStrict =  Binary.decode . Lazy.fromStrict

encodeStrict :: (Binary a) =>  a -> Eager.ByteString
encodeStrict =  Lazy.toStrict . Binary.encode 

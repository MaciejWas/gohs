{-# LANGUAGE DeriveGeneric #-}

module Lib.DataModel.UserModel (UserRegistered(..), UserData(..)) where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)
import Lib.DataModel.AuthModel (UserName)

type ID = Int

type Time = Int

data UserRegistered = UserCreated ID UserName Time
                    deriving (Show, Eq, Generic)
instance ToJSON UserRegistered
instance FromJSON UserRegistered

data UserData = UserData
  { id :: ID,
    registered :: Time,
    uname :: UserName
  } deriving (Show, Eq, Generic)
instance ToJSON UserData
instance FromJSON UserData

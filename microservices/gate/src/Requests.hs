{-# LANGUAGE DeriveGeneric #-}

module Requests where
import Data.Aeson ( FromJSON, ToJSON )
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Eager
import GHC.Generics (Generic)


data HTTPAuthRequest
  = HTTPAuthRequest { login :: Lazy.Text, pswd :: Lazy.Text } deriving Generic
instance FromJSON HTTPAuthRequest

data HTTPAuthResponse
  = HTTPAuthResponse { success :: Bool, userId :: Maybe Int, msg :: Eager.Text } deriving Generic
instance ToJSON HTTPAuthResponse
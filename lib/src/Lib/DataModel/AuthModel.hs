{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This is a place for all datastructures used by the Auth microservice and friends
module Lib.DataModel.AuthModel where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.UnixTime (UnixTime (UnixTime), getUnixTime)
import Lib.Errors (AppResult)
import Network.Socket (HostAddress, HostAddress6)
import Relude as Re (Generic, MonadIO (liftIO), Text)

type ID = Int
type UnixTimeSecs = Int

newtype Password = PswdAsText Lazy.Text deriving (Eq, Show, Generic)
instance Binary Password
instance ToJSON Password
instance FromJSON Password

newtype UserName = UserName Lazy.Text deriving (Eq, Show, Generic)
instance Binary UserName
instance ToJSON UserName
instance FromJSON UserName

data AuthRequest
  = RegisterRequest ID RegisterRequestData
  | LoginRequest ID LoginRequestData
  | CheckRights ID CheckRightsData
  deriving (Show, Generic)
instance ToJSON AuthRequest
instance FromJSON AuthRequest

data RegisterRequestData = RegisterRequestData UserName Password 
                         deriving (Show, Generic)
instance ToJSON RegisterRequestData
instance FromJSON RegisterRequestData

data LoginRequestData = LoginRequestData IPAddr UserName Password 
                      deriving (Show, Generic)
instance ToJSON LoginRequestData
instance FromJSON LoginRequestData

data CheckRightsData = CheckRightsData IPAddr Session
                     deriving (Show, Generic)
instance ToJSON CheckRightsData
instance FromJSON CheckRightsData

-- | Result of authorization
data AuthResponse
  = LoginResponse ID LoginResponseData
  | RegisterResponse ID RegisterResponseData
  | RightsResponse ID (Maybe Rights)
  deriving (Show, Generic)
instance ToJSON AuthResponse
instance FromJSON AuthResponse

data LoginResponseData
  = LoginResponseOK UserName Int Session
  | LoginResponseFailed Text
  deriving (Show, Generic)
instance ToJSON LoginResponseData
instance FromJSON LoginResponseData

data RegisterResponseData
  = RegisterResponseOK UserName Int
  | RegisterResponseFailed Text
  deriving (Show, Generic)
instance ToJSON RegisterResponseData
instance FromJSON RegisterResponseData

idOf :: AuthResponse -> ID
idOf (LoginResponse x _) = x
idOf (RegisterResponse x _) = x
idOf (RightsResponse x _) = x

-- | Everybody knows what a session is, right?
newtype Session = Session UUID deriving (Show, Eq, Generic)
instance Binary Session
instance ToJSON Session
instance FromJSON Session

data SessionData = SessionData Rights Int deriving (Show, Eq, Generic)
instance Binary SessionData
instance ToJSON SessionData
instance FromJSON SessionData

createSession :: Rights -> AppResult (Maybe (Session, SessionData))
createSession rights = liftIO $ do
  uuid <- nextUUID
  (UnixTime secs _msecs) <- getUnixTime
  let expireTime = (fromEnum secs :: Int) + 60 * 60 * 12
  return $ case uuid of
    Nothing -> Nothing
    Just uuid' -> Just (Session uuid', SessionData rights expireTime)

-- | Priveledges of a given API user
data Rights
  = LoggedAs IPAddr ID
  | LoggedOut
  deriving (Show, Eq, Generic)
instance ToJSON Rights
instance FromJSON Rights
instance Binary Rights

data IPAddr
  = V6 HostAddress6
  | V4 HostAddress
  | Unix String
  deriving (Show, Eq, Generic)
instance Binary IPAddr
instance ToJSON IPAddr
instance FromJSON IPAddr

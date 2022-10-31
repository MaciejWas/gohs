-- | This is a place for all datastructures used by the Auth microservice and friends

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.DataModel.AuthModel where

    import Data.Aeson ( FromJSON, ToJSON )
    import Data.UUID.V1 (nextUUID)
    import Data.UUID (UUID)
    import qualified Data.Text.Lazy as Lazy
    import Data.Binary (Binary)
    import Network.Socket (HostAddress6, HostAddress)
    import Lib.Errors (AppErr(..), canBeShownToUser, AppResult)
    import Relude as Re
    
    type ID = Int

    newtype UserId = UserId Int deriving (Eq, Show, Generic)
    instance Binary UserId
    instance ToJSON UserId
    instance FromJSON UserId

    newtype Password = PswdAsText Lazy.Text deriving (Eq, Generic)
    instance Binary Password
    instance ToJSON Password
    instance FromJSON Password

    newtype UserName = UserName Lazy.Text deriving (Eq, Generic)
    instance Binary UserName
    instance ToJSON UserName
    instance FromJSON UserName

    data AuthAction = Login | Register deriving (Generic)
    instance Binary AuthAction

    -- | This structure is used both for registration and authentication
    data AuthRequest = AuthRequest ID AuthAction UserName Password
                     deriving (Generic)
    instance Binary AuthRequest
    
    -- | Result of authorization
    data AuthResponse = AuthOk ID Session | AuthFailed ID Text deriving (Generic)
    instance Binary AuthResponse

    -- | Everybody knows what a session is, right?
    newtype Session = Session UUID deriving (Show, Eq, Generic)
    instance ToJSON Session
    instance FromJSON Session
    instance Binary Session

    nextSession :: AppResult (Maybe Session)
    nextSession = liftIO $ nextUUID >>= return . fmap Session

    -- | Priveledges of given API user
    data Rights = LoggedAs UserId
                | LoggedOut
                deriving (Show, Eq, Generic)
    instance ToJSON Rights
    instance FromJSON Rights
    instance Binary Rights

    data IPAddr = V6 HostAddress6
                | V4 HostAddress
                | Unix String
                deriving (Show, Eq, Generic)
    instance Binary IPAddr

{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Gate.DataModel where
    import Data.Aeson (ToJSON, FromJSON)
    import GHC.Generics (Generic)

    import Lib.DataModel.AuthModel (Password, UserName)
    import Data.Text (Text)
    
    data AuthRequest = Register UserName Password
                     | Login    UserName Password
                     deriving (Generic)
    instance ToJSON AuthRequest
    instance FromJSON AuthRequest

    data MsgResponse = MsgResponse { msg :: Text } deriving (Generic)
    instance ToJSON MsgResponse
    instance FromJSON MsgResponse

    
    

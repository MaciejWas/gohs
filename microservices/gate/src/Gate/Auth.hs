{-# LANGUAGE OverloadedStrings #-}

module Gate.Auth (loginEndpointAction, registerEndpointAction) where
  import Network.HTTP.Types.Header (hSetCookie)
  import Network.HTTP.Types.Status (status200)
  import Network.Wai (Request, Response, strictRequestBody)
  import Data.Aeson (FromJSON, encode, decode)
  import qualified Data.ByteString.Lazy as Lazy
  import Control.Monad.IO.Class (MonadIO(..))

  import Gate.Endpoint (findIP, sendBack)
  import Lib.Errors (AppResult, withMessage, ErrType (..), AppErr, unwrap)
  import Lib.DataModel.AuthModel (Rights)
  import Lib.Queue (postMsg)

  
  
  loginEndpointAction :: Request -> Rights -> AppResult Response
  loginEndpointAction req _ = do
    -- body <- liftIO $ strictRequestBody req
    -- authReq <- unwrap (deserialize body)
    sendBack status200 [(hSetCookie, "token=" <> "TODO: Kura tutaj token")] ()
    
  registerEndpointAction :: Request -> Rights -> AppResult Response
  registerEndpointAction req  _ = do
    -- body <- liftIO $ strictRequestBody req
    -- authReq <- unwrap (deserialize body)
    sendBack status200 [] ()

  deserialize :: (FromJSON a) => Lazy.ByteString -> Either AppErr a
  deserialize serialized = case decode serialized of
    Just deserialized -> return deserialized
    Nothing ->  Left (BadReq `withMessage` "Could not deserialize")



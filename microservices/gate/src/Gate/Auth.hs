{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Gate.Auth (loginEndpointAction, registerEndpointAction) where
  import Control.Monad.IO.Class (MonadIO(..))

  import Network.HTTP.Types.Header (hSetCookie)
  import Network.HTTP.Types.Status (status200, status500)
  import Network.Wai (Request, Response, strictRequestBody)

  import Data.Aeson (FromJSON, decode)
  import qualified Data.ByteString.Lazy as Lazy
  import Data.UUID (toASCIIBytes)
  import Data.Text (pack)

  import qualified Lib.Log
  import Lib.Errors (AppResult, withMessage, ErrType (..), AppErr, unwrap)
  import Lib.DataModel.AuthModel (Rights, AuthRequest(..), UserName (UserName), Password (PswdAsText), AuthResponse (..), Session (Session), idOf, LoginResponseData (..), RegisterResponseData (..), LoginRequestData (LoginRequestData), RegisterRequestData (RegisterRequestData))
  import Lib.Stream (send, watch)
  import Lib.IdMap (createNewUniqueId, getNextId)
  import Lib.Persistence (RedisAddr)
  import Api ()

  import Gate.Endpoint (sendBack, findIP)
  import Requests (HTTPAuthRequest(..), HTTPAuthResponse (HTTPAuthResponse))

  authRequestId :: RedisAddr () Int
  authRequestId = createNewUniqueId "auth"
  
  loginEndpointAction :: Request -> AppResult Response
  loginEndpointAction req = do
    body <- liftIO $ strictRequestBody req
    (HTTPAuthRequest usrname passwd) <- unwrap (deserialize body)
    cid <- getNextId authRequestId

    liftIO $ Lib.Log.info $ "Received new request. Assigning authid=" ++ show cid
    let ipaddr = findIP req
    let authRequest = LoginRequest cid (LoginRequestData ipaddr (UserName usrname) (PswdAsText passwd))
    send authRequest

    response <- watch (\response -> cid == idOf response) return
    case response of
      (LoginResponse _ rrdata) -> case rrdata of
        LoginResponseOK _username _userid (Session sess) -> sendBack status200 [(hSetCookie, "token=" <> toASCIIBytes sess)] (HTTPAuthResponse True Nothing "Logged in.")
        LoginResponseFailed txt -> sendBack status500 [] (HTTPAuthResponse False Nothing txt)
      _ -> unexpectedResponse
    
  registerEndpointAction :: Request -> AppResult Response
  registerEndpointAction req = do
    body <- liftIO $ strictRequestBody req
    (HTTPAuthRequest usrname passwd) <- unwrap (deserialize body)
    cid <- getNextId authRequestId

    liftIO (Lib.Log.info ("Received new register request. Assigning authid=" ++ show cid))

    let authRequest = RegisterRequest cid (RegisterRequestData (UserName usrname) (PswdAsText passwd))

    send authRequest

    liftIO (Lib.Log.info ("Awaiting response for authid=" ++ show cid))

    response <- watch (\response -> cid == idOf response) return

    liftIO (Lib.Log.info ("Response received2 for authid=" ++ show cid))

    case response of
      RegisterResponse _ d -> case d of
        RegisterResponseOK _userName userId -> sendBack status200 [] (HTTPAuthResponse True (Just userId) (pack ("registered " ++ show userId)))
        RegisterResponseFailed txt -> sendBack status500 [] (HTTPAuthResponse False Nothing txt)
      _ -> unexpectedResponse

  deserialize :: (FromJSON a) => Lazy.ByteString -> Either AppErr a
  deserialize serialized = case decode serialized of
    Just deserialized -> return deserialized
    Nothing ->  Left (BadReq `withMessage` "Could not deserialize")


  unexpectedResponse :: AppResult Response
  unexpectedResponse = liftIO (Lib.Log.err "Couldn't match auth response type") >> sendBack status500 [] (HTTPAuthResponse False Nothing "Internal error.")



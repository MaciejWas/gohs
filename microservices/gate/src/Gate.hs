{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Gate
  ( handle,
  )
where

import GHC.Generics (Generic)

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON)

import Network.HTTP.Types (Header, hAccept, hContentType, methodGet, status200, status404, status405, status500, methodPost)
import Network.Wai (Request, Response, mapResponseHeaders)

import Lib.Errors (AppErr (AppErr), AppResult, ErrType (..), canBeShownToUser)
import Lib.Log (err)

import Gate.Endpoint (Endpoint (Endpoint), findEndpointFor, sendBack)
import Gate.Games (getGamesEndpoint)
import Gate.Auth (loginEndpointAction, registerEndpointAction)

endpoints :: [Endpoint]
endpoints =
  [ Endpoint ["auth", "register"] methodPost registerEndpointAction [ hAcceptJSON, hContentUTF8JSON ],
    Endpoint ["auth", "login"]    methodPost loginEndpointAction    [ hAcceptJSON, hContentUTF8JSON ],
    Endpoint ["ping"]             methodGet  pingEndpoint           [ hAcceptAll,  hContentUTF8JSON ],
    Endpoint ["games"]            methodGet  getGamesEndpoint       []
  ]

handle :: Request -> AppResult Response
handle req = tryToHandle req `catchError` handleErr

tryToHandle :: Request -> AppResult Response
tryToHandle req = case findEndpointFor endpoints req of
  Left apperr -> throwError apperr
  Right (Endpoint _ _ process headers) ->
    let endpointResult = process req
        addEndpointHeaders = mapResponseHeaders (++ headers)
     in addEndpointHeaders <$> endpointResult

handleErr :: AppErr -> AppResult Response
handleErr (AppErr errtype errmsg) = do
  liftIO $ err (show errmsg)
  let responseMsg = if errtype `elem` canBeShownToUser then errmsg else "Internal error."
  let status = case errtype of
        NotFound -> status404
        BadMethod -> status405
        _ -> status500
  sendBack status [] responseMsg


pingEndpoint :: Request -> AppResult Response
pingEndpoint _ = sendBack status200 [] ("pong" :: String)

-- Responses

data BasicResponse a = BasicResponse { success :: Bool, msg :: String, info :: Maybe a } deriving (Generic)

instance (ToJSON a) => ToJSON (BasicResponse a)

-- HEADERS
hAcceptJSON :: Header
hAcceptJSON = (hAccept, "application/json")

hContentUTF8JSON :: Header
hContentUTF8JSON = (hContentType, "application/json; charset=utf-8")

hAcceptAll :: Header
hAcceptAll = (hAccept, "*/*")

-- UTILS

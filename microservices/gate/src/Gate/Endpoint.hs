{-# LANGUAGE OverloadedStrings #-}

module Gate.Endpoint (Endpoint(..), findEndpointFor, sendBack, findIP, getToken) where
    import Web.Cookie (parseCookies)
    import Network.HTTP.Types (Method)
    import Network.HTTP.Types.Header (Header)
    import Network.HTTP.Types.Status (Status)
    import Network.Wai (Request (..), Response, requestMethod, responseLBS)
    import Network.Socket (SockAddr(..))

    import Data.Aeson (ToJSON, encode)
    import Data.Text (Text)
    import Data.ByteString (ByteString)

    import Lib.Errors (AppResult, withMessage, ErrType (..), AppErr)
    import Lib.DataModel.AuthModel (IPAddr (..))
    import Data.UUID (UUID, fromASCIIBytes)
    
    data Endpoint = Endpoint {
        _pathOf :: [Text], -- | path to endpoint. E.g. /users/0/data
        _method :: Method,
        _action :: Request -> AppResult Response,
        _headers :: [Header]
    }

    type Cookie = (ByteString, ByteString)

    findEndpointFor :: [Endpoint] -> Request -> Either AppErr Endpoint
    findEndpointFor endpoints request = do
        let methodMatches req (Endpoint _ method _ _) = requestMethod req == method
        let pathMatches req (Endpoint path _ _ _)     = path == pathInfo req
        let matchingPathEndpoints = filter (pathMatches request) endpoints
        matchingMetEndpoints <- if null matchingPathEndpoints then Left (NotFound `withMessage` "Endpoint does not exist")
                                                            else Right $ filter (methodMatches request) matchingPathEndpoints
        case matchingMetEndpoints of
            [] -> Left (BadMethod `withMessage` "Bad Method")
            x:_ -> Right x

    findIP :: Request -> IPAddr
    findIP req = case remoteHost req of
        SockAddrInet _ hostaddr      -> V4 hostaddr
        SockAddrInet6 _ _ hostaddr _ -> V6 hostaddr
        SockAddrUnix str             -> Unix str

    sendBack :: (ToJSON a) => Status -> [Header] -> a -> AppResult Response
    sendBack status headers = return . responseLBS status headers . encode

    getCookies :: Request -> [Cookie]
    getCookies req = let headers = requestHeaders req
                         cookies = case filter ((=="Cookie") . fst) headers of (_header, cs):_ -> cs
                                                                               _ -> ""
                     in parseCookies cookies

    getTokenBytes :: [Cookie] -> Maybe ByteString
    getTokenBytes cookies = case filter isToken cookies of
      (_key, value):_ -> Just value
      []              -> Nothing


    getToken :: Request -> Maybe UUID
    getToken req = (getTokenBytes . getCookies) req >>= fromASCIIBytes

    isToken :: Cookie -> Bool
    isToken (key, _val) = key == "token"







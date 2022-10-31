module Gate.Games where
  import Network.Wai (Request, Response)
  import Network.HTTP.Types (status200)

  import Lib.Errors (AppResult)
  import Gate.Endpoint (sendBack)
  import Lib.DataModel.AuthModel (Rights)
  
  getGamesEndpoint :: Request -> Rights -> AppResult Response
  getGamesEndpoint _req vrights = undefined
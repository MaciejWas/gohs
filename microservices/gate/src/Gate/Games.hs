module Gate.Games where
  import Network.Wai (Request, Response)

  import Lib.Errors (AppResult)
  
  getGamesEndpoint :: Request -> AppResult Response
  getGamesEndpoint _req = undefined
module Main (main) where

import System.Environment (lookupEnv)

import AuthWorker (worker)
import Database.Redis (checkedConnect, defaultConnectInfo, ConnectInfo(..), PortID(..))
import Lib.Log (info)
import Lib.Worker (run)
import Api 


orIfEmpty :: Maybe a -> a -> a
orIfEmpty (Just a) _ = a
orIfEmpty Nothing a = a

loadConnInfo :: IO ConnectInfo
loadConnInfo = do
  hostenv <- lookupEnv "REDIS_HOST" 
  let host = hostenv `orIfEmpty` "localhost"

  portenv <- lookupEnv "REDIS_PORT" 
  let port = case portenv of Nothing -> PortNumber 6379
                             Just text -> PortNumber (read text)
  return (defaultConnectInfo { connectHost = host, connectPort = port })


main :: IO ()
main = do
  conninfo <- loadConnInfo
  info $ show conninfo
  conn <- checkedConnect conninfo
  run conn worker

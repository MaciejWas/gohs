module Main (main) where

import AuthWorker (runner)
import Database.Redis (checkedConnect, defaultConnectInfo)
import Lib.Log (info)
import Lib.Queue (run)

main :: IO ()
main = do
  info "Starting new microservice"
  conn <- checkedConnect defaultConnectInfo
  run conn runner

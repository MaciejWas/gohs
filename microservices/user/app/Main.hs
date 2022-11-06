module Main (main) where

import AuthWorker (worker)
import Database.Redis (checkedConnect, defaultConnectInfo)
import Lib.Log (info)
import Lib.Worker (run)
import Api 

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  run conn worker

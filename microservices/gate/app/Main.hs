{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where
  import Network.Wai ( Application)
  import qualified Network.Wai.Handler.Warp as Warp
  import Data.Text ( Text )
  import qualified Network.WebSockets as WS
  import qualified Network.Wai.Handler.WebSockets as WaiWS
  import Control.Monad.Except ( runExceptT )
  import Database.Redis ( runRedis, checkedConnect, defaultConnectInfo, Connection )

  import Gate ( handle )
  import Lib.Log (info)
  
  type RedisConnection = Connection

  port :: Int
  port = 3000

  settings :: Warp.Settings
  settings = makeSettings [ Warp.setPort port
                          ]

  main :: IO ()
  main = info "Starting gate server" >> getRedisConnection >>= startAppWithRedisConn

  getRedisConnection :: IO RedisConnection
  getRedisConnection = checkedConnect defaultConnectInfo

  startAppWithRedisConn :: RedisConnection -> IO ()
  startAppWithRedisConn conn
    = let httpApp = handleHttpReq conn
          wsApp = websocketsApp conn
          app = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp httpApp
      in Warp.runSettings settings app

  websocketsApp :: RedisConnection -> WS.PendingConnection -> IO ()
  websocketsApp _redisConn pending_conn = do
      conn <- WS.acceptRequest pending_conn
      WS.sendTextData conn ("Hello, client!" :: Text)

  handleHttpReq :: RedisConnection -> Application
  handleHttpReq redisConn req respond = do
    let dbOperation =  runExceptT $ handle req
    result <- runRedis redisConn dbOperation
    case result of {
      Right r -> respond r;
    }

  -- HELPERS ----

  makeSettings :: [ Warp.Settings -> Warp.Settings ] -> Warp.Settings
  makeSettings settings' = foldl (.) id settings' Warp.defaultSettings
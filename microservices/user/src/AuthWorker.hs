{-# LANGUAGE OverloadedStrings #-}

module AuthWorker (worker) where

import Auth (login, register, checkRights)
import Lib.DataModel (AuthRequest(..))
import Lib.DataModel.AuthModel (AuthResponse)
import Lib.Errors (AppResult)
import Lib.Worker (StreamWorker (..))

mainAuthAction :: () -> AuthRequest -> AppResult AuthResponse
mainAuthAction _ req = case req of
  RegisterRequest n rreq -> register n rreq
  LoginRequest n lreq -> login n lreq
  CheckRights n crreq -> checkRights n crreq

worker :: StreamWorker AuthRequest AuthResponse ()
worker =
  StreamWorker
    { name = "auth-worker",
      loadState = return (),
      ms_main = mainAuthAction
    }
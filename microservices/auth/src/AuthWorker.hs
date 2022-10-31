{-# LANGUAGE OverloadedStrings #-}

module AuthWorker (runner) where

import Auth (login, register)
import Control.Monad.Except (MonadError (catchError))
import Data.Binary (encode)
import Lib.DataModel (AuthRequest (AuthRequest))
import Lib.DataModel.AuthModel (AuthAction (..), AuthResponse)
import Lib.Errors (AppResult, FromError (fromError))
import Lib.Log (logError)
import Lib.Queue (QueueWorker (..), RedisQueue (..))

mainAuthAction :: () -> AuthRequest -> AppResult AuthResponse
mainAuthAction _ (AuthRequest _cid action uname pswd) = case action of
  Register -> catchError (register uname pswd) handleErr
  Login -> catchError (login uname pswd) handleErr
  where
    handleErr err = logError err >> fromError err

runner :: QueueWorker AuthRequest AuthResponse ()
runner =
  QueueWorker
    { name = "Auth service",
      listen_to = RedisQueue "auth:requests",
      publish_to = RedisQueue "auth:reponses",
      loadState = return (),
      findSubQueue = \(AuthRequest cid _ _ _) -> encode cid, -- lazy!
      ms_main = mainAuthAction
    }

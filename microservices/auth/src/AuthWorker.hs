{-# LANGUAGE OverloadedStrings #-}

module AuthWorker (runner) where

import Auth (login, register)
import Control.Monad.Except (MonadError (catchError))
import Data.Binary (encode)
import Lib.DataModel (AuthRequest (AuthRequest))
import Lib.DataModel.AuthModel (AuthAction (..), AuthResponse (AuthFailed))
import Lib.Errors (AppResult, AppErr (AppErr), canBeShownToUser)
import Lib.Log (logError)
import Lib.Queue
import Lib.Worker (StreamWorker (..))

type AuthRequestID = Int

mainAuthAction :: () -> AuthRequest -> AppResult AuthResponse
mainAuthAction _ (AuthRequest cid action uname pswd) = case action of
  Register -> catchError (register cid uname pswd) handleErr
  Login -> catchError (login cid uname pswd) handleErr
  where
    handleErr err = logError err >> return (fromError err cid)

fromError :: AppErr -> AuthRequestID -> AuthResponse
fromError (AppErr errtype errmsg) cid = let message = if errtype `elem` canBeShownToUser then errmsg else "Internal error during authorization"
                                        in AuthFailed cid message

runner :: StreamWorker AuthRequest AuthResponse ()
runner =
  StreamWorker
    { name = "authworker",
      loadState = return (),
      ms_main = mainAuthAction
    }

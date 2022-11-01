{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth (login, register) where

import Control.Monad.Except (catchError, liftIO, throwError)
import Data.Text.Lazy.Encoding (encodeUtf8)
import IdMap (getNextId)
import Lib.DataModel.AuthModel
  ( AuthAction (..),
    AuthRequest (..),
    AuthResponse (AuthFailed, AuthOk),
    Password (..),
    Rights (..),
    Session (Session),
    UserId,
    UserName (UserName),
    nextSession,
  )
import Lib.Errors
  ( AppErr,
    AppResult,
    ErrType (..),
    orThrowIfEmpty,
    unwrap,
    unwrapMaybe,
    withMessage,
  )
import Lib.Persistence (RedisAddr (..), exists, get, set)
import Security.Passwords (Hashed, IsValid (..), check, hash)

type AuthRequestID = Int

userPswd :: RedisAddr (Hashed Password)
userPswd = RedisAddr "pswd:"

userId :: RedisAddr UserId
userId = RedisAddr "user:id:"

sessions :: RedisAddr Rights
sessions = RedisAddr "session:"

register :: AuthRequestID -> UserName -> Password -> AppResult AuthResponse
register idx userName@(UserName uname) pswd = do
  let utf8EncodedLogin = encodeUtf8 uname
  userExists <- exists userId utf8EncodedLogin
  if userExists
    then throwError userExistsError
    else continue
  hashedPswd <- unwrap (hash pswd)
  userId <- getNextId
  set userPswd userId hashedPswd
  login idx userName pswd

registerAdmin :: AppResult AuthResponse
registerAdmin = register 0 (UserName "admin") (PswdAsText "1234")

login :: AuthRequestID -> UserName -> Password -> AppResult AuthResponse
login idx (UserName uname) pswd = do
  let utf8EncodedLogin = encodeUtf8 uname
  uid <- get userId utf8EncodedLogin `orThrowIfEmpty` userDoesNotExist
  storedPswd <- get userPswd uid `orThrowIfEmpty` pswdNotFound
  sess <- nextSession `orThrowIfEmpty` (InternalErr `withMessage` "session failed")
  let rights = LoggedAs uid
  set sessions sess rights

  case check pswd storedPswd of
    IsValid -> return (AuthOk idx sess)
    IsNotValid -> throwError wrongPswd

-- ERRORS --------------

userExistsError :: AppErr
userExistsError = AuthError `withMessage` "User already exists!"

wrongPswd :: AppErr
wrongPswd = AuthError `withMessage` "Wrong password!"

userDoesNotExist :: AppErr
userDoesNotExist = AuthError `withMessage` "User does not exist!"

pswdNotFound :: AppErr
pswdNotFound = DBError `withMessage` "password not found (very weird)"

-- UTILS ---------------

continue :: AppResult ()
continue = return ()

-- REALMS --------------

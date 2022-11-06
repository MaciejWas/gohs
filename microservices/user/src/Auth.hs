{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Auth (login, register, checkRights) where

import Control.Monad.Except (throwError, MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Lib.IdMap (getNextId, createNewUniqueId)
import Lib.DataModel.AuthModel
  ( AuthResponse (..),
    Password (..),
    Rights (..),
    UserName (UserName),
    createSession, Session (Session), RegisterRequestData (RegisterRequestData), RegisterResponseData (..), LoginRequestData (LoginRequestData), LoginResponseData (LoginResponseOK, LoginResponseFailed), CheckRightsData (CheckRightsData), IPAddr, SessionData (SessionData),
  )
import Data.UnixTime (getUnixTime, UnixTime (..))
import Lib.Errors
  ( AppErr (AppErr),
    AppResult,
    ErrType (..),
    orThrowIfEmpty,
    unwrap,
    withMessage, canBeShownToUser,
  )
import Lib.Persistence (RedisAddr (..), exists, get, set)
import Security.Passwords (Hashed, Validity (..), check, hash)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as Lazy
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.UUID (UUID)

type AuthRequestID = Int
type UserID = Int

newtype UserNameUTF8 = UserNameUTF8 Lazy.ByteString deriving Generic
instance Binary UserNameUTF8

-- Redis

maxUserId :: RedisAddr () UserID
maxUserId = createNewUniqueId "user:id:"

userPswd :: RedisAddr UserID (Hashed Password)
userPswd = RedisAddr "user:pswd:"

usersIds :: RedisAddr UserNameUTF8 UserID
usersIds = RedisAddr "user:id:"

userSession :: RedisAddr UserID Session
userSession = RedisAddr "user:session:"

--

sessionData :: RedisAddr Session SessionData
sessionData = RedisAddr "session:data:"

-- 

register' :: AuthRequestID -> RegisterRequestData -> AppResult AuthResponse
register' requestId (RegisterRequestData  userName pswd) = do
  userExists <- exists usersIds (utf8 userName)
  if userExists
    then throwError userExistsError
    else continue

  hashedPswd <- unwrap (hash pswd)

  userId <- getNextId maxUserId
  set usersIds (utf8 userName) userId
  set userPswd userId hashedPswd

  return (RegisterResponse requestId (RegisterResponseOK userName userId))

register :: AuthRequestID -> RegisterRequestData -> AppResult AuthResponse
register requestId rdata = catchError
  (register' requestId rdata)
  (return . RegisterResponse requestId . RegisterResponseFailed . messageOf)

login' :: AuthRequestID -> LoginRequestData -> AppResult AuthResponse
login' requestId (LoginRequestData ipaddr userName pswd) = do
  userId <- get usersIds (utf8 userName)
    `orThrowIfEmpty` userDoesNotExist
  storedPswd <- get userPswd userId
    `orThrowIfEmpty` pswdNotFound

  case check pswd storedPswd of
    IsValid -> do
      sess <- createOrRetrieveSession requestId ipaddr userName
      return $ LoginResponse requestId (LoginResponseOK userName userId sess)
    IsNotValid -> throwError wrongPswd

login :: AuthRequestID -> LoginRequestData -> AppResult AuthResponse
login requestId rdata = catchError
  (login' requestId rdata)
  (return . LoginResponse requestId . LoginResponseFailed . messageOf)

createOrRetrieveSession :: AuthRequestID -> IPAddr -> UserName -> AppResult Session
createOrRetrieveSession requestId ipaddr userName = do
    userId <- get usersIds (utf8 userName) `orThrowIfEmpty` (InternalErr `withMessage` Data.Text.pack ("Could not find user while processing id=" ++ show requestId))
    currSess <- get userSession userId
    case currSess of
      Nothing -> createNewSession requestId (LoggedAs ipaddr userId)
      Just sess -> do
        currTimeSecs <- currentUnixTime
        (SessionData rights expireTime) <- get sessionData sess `orThrowIfEmpty` (InternalErr `withMessage` Data.Text.pack ("Could not find session data for request id=" ++ show requestId))
        if expireTime > currTimeSecs then return sess else createNewSession requestId rights

createNewSession :: AuthRequestID -> Rights -> AppResult Session
createNewSession requestId rights = do
  (session, sessData) <- createSession rights `orThrowIfEmpty` (InternalErr `withMessage` Data.Text.pack ("Generating session failed for request id=" ++ show requestId))
  set sessionData session sessData
  return session

checkRights :: AuthRequestID -> CheckRightsData -> AppResult AuthResponse
checkRights requestId (CheckRightsData ipaddr sess) = do
  sessData <- get sessionData sess
  currTimeSecs <- currentUnixTime
  rights <- case sessData of
    Nothing -> return LoggedOut
    Just (SessionData rights expireTime) -> if expireTime > currTimeSecs then return rights else return LoggedOut
  
  return (RightsResponse requestId (Just rights))

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

utf8 :: UserName -> UserNameUTF8
utf8 (UserName uname) = UserNameUTF8 (encodeUtf8 uname)

messageOf (AppErr ttype text) = if ttype `elem` canBeShownToUser then text else "Internal error"

currentUnixTime :: AppResult Int
currentUnixTime = do
   (UnixTime secs _msecs) <- liftIO getUnixTime
   let currTimeSecs = fromEnum secs
   return currTimeSecs

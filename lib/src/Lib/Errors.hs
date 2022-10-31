{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Errors
  ( ErrType (..),
    AppErr (..),
    AppResult,
    canBeShownToUser,
    withMessage,
    doWithRedis,
    orThrowIfEmpty,
    orThrowIfFailed,
    unwrap,
    unwrapDb,
    unwrapMaybe,
    dbFailedErr,
    authFailedErr,
    orElseThrow,
  )
where

import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Text (Text, pack)
import Database.Redis (Redis)

data ErrType
  = Empty
  | InternalErr
  | BadReq
  | BadMethod
  | AuthError
  | NotFound
  | GameError
  | MethodNotAllowed
  | DBError
  deriving (Show, Eq)

data AppErr = AppErr ErrType Text deriving (Show, Eq)

type AppResult a = ExceptT AppErr Redis a

canBeShownToUser :: [ErrType]
canBeShownToUser = [BadReq, AuthError, GameError, NotFound, MethodNotAllowed]

withMessage :: ErrType -> Text -> AppErr
withMessage = AppErr

doWithRedis :: Redis a -> AppResult a
doWithRedis = lift

orThrowIfFailed :: (Show err) => Either err a -> ErrType -> AppResult a
orThrowIfFailed result errtype = case result of
  Left errmsg -> throwError (errtype `withMessage` (pack . show) errmsg)
  Right ok -> return ok

orElseThrow :: AppResult (Either err a) -> AppErr -> AppResult a
orElseThrow result err =
  result >>= \case
    Left _ -> throwError err
    Right ok -> return ok

orThrowIfEmpty :: AppResult (Maybe a) -> AppErr -> AppResult a
orThrowIfEmpty m apperr =
  m >>= \case
    Nothing -> throwError apperr
    Just val -> return val

unwrap :: Either AppErr a -> AppResult a
unwrap (Left err) = throwError err
unwrap (Right ok) = return ok

unwrapMaybe :: Maybe a -> AppErr -> AppResult a
unwrapMaybe Nothing err = throwError err
unwrapMaybe (Just ok) _ = return ok

unwrapDb :: Either err ok -> AppResult ok
unwrapDb x = case x of
  Right ok -> return ok
  Left _err -> throwError dbFailedErr

-- CONCRETE ERRORS

dbFailedErr :: AppErr
dbFailedErr = DBError `withMessage` "Redis operation failed"

authFailedErr :: AppErr
authFailedErr = AuthError `withMessage` "Authentication failed"

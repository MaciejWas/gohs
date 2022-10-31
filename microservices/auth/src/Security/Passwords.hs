{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Security.Passwords (Hashed(..), Password(..), hash, IsValid(..), check) where
  import Data.ByteString (ByteString)
  import Data.Text (pack)
  import GHC.Generics (Generic)
  import Data.Binary ( Binary, encode )

  import qualified Crypto.KDF.Argon2 as A
  import Crypto.Error (CryptoFailable(CryptoPassed, CryptoFailed))
  import Data.ByteString.Lazy (toStrict)

  import Lib.DataModel.AuthModel ( Password(..) )
  import Lib.Errors (withMessage, ErrType (BadReq), AppErr)

  newtype Hashed a = BSHash ByteString deriving (Eq, Generic)
  data IsValid = IsValid | IsNotValid

  check :: Password -> Hashed Password -> IsValid
  check pswd realPswdHash = case hash pswd of
    Left _err -> IsNotValid
    Right val | val == realPswdHash -> IsValid
              | otherwise           -> IsNotValid

  salt :: ByteString
  salt = "7586642910"

  outputLen :: Int
  outputLen = 10

  defaultOptions :: A.Options
  defaultOptions
    = A.Options { A.iterations  = 1
                , A.memory      = 2 ^ (17 :: Int)
                , A.parallelism = 8
                , A.variant     = A.Argon2d
                , A.version     = A.Version13
                }

  hash :: Password -> Either AppErr (Hashed Password)
  hash (PswdAsText pswd)
   = let bytes = (toStrict . encode) pswd
     in case A.hash defaultOptions bytes salt outputLen of
       CryptoPassed x -> Right (BSHash x)
       CryptoFailed ce -> Left (BadReq `withMessage` (pack . show) ce)

  instance (Binary a) => Binary (Hashed a);





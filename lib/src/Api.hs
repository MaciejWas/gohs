{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Api () where
  import Lib.Stream (Stream(Stream), StreamMessage (..))
  import Lib.DataModel.AuthModel ( AuthRequest (..), AuthResponse(..) )


  instance StreamMessage AuthRequest where
    stream :: Stream AuthRequest
    stream = Stream "auth:request"

  instance StreamMessage AuthResponse where
    stream :: Stream AuthResponse
    stream = Stream "auth:response"
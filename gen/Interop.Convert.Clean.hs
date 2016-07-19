{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Convert.Clean where




import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default
import           Data.Int            (Int64)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Typeable       (Typeable)
import           Data.Monoid         ((<>))
import           GHC.Generics        (Generic)
import           Haskell.Api.Helpers (QueryParam, qp)

userRequestToUserResponse :: Int64 -> Bool -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse id active createdAt modifiedAt UserRequest{..} =
  UserResponse {
    id = id,
    name = name,
    email = email,
    active = active,
    createdAt = createdAt,
    modifiedAt = modifiedAt
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    name = name,
    email = email
  }

-- footer
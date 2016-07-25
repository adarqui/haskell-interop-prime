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




import           Control.DeepSeq             (NFData)
import           Data.Aeson                  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default                (Default, def)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime)
import           Data.Typeable               (Typeable)
import           Data.Monoid                 ((<>))
import           GHC.Generics                (Generic)
import           Haskell.Api.Helpers.Shared  (QueryParam, qp)

userRequestToUserResponse :: Int64 -> Bool -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse _1 _2 _3 _4 UserRequest{..} =
  UserResponse {
    id = _1,
    active = _2,
    createdAt = _3,
    modifiedAt = _4,
    userResponsename = name,
    userResponseemail = email
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    userRequestname = name,
    userRequestemail = email
  }

-- footer
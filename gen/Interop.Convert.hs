{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Convert where




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
    userResponseId = _1,
    userResponseActive = _2,
    userResponseCreatedAt = _3,
    userResponseModifiedAt = _4,
    userResponseName = userRequestName,
    userResponseEmail = userRequestEmail
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    userRequestName = userResponseName,
    userRequestEmail = userResponseEmail
  }

-- footer
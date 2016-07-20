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

userRequestToUserResponse :: Int64 -> String -> String -> Bool -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse userResponseId userResponseName userResponseEmail userResponseActive userResponseCreatedAt userResponseModifiedAt UserRequest{..} =
  UserResponse {
    userResponseId = userResponseId,
    userResponseName = userResponseName,
    userResponseEmail = userResponseEmail,
    userResponseActive = userResponseActive,
    userResponseCreatedAt = userResponseCreatedAt,
    userResponseModifiedAt = userResponseModifiedAt
  }


userResponseToUserRequest :: String -> String -> UserResponse -> UserRequest
userResponseToUserRequest userRequestName userRequestEmail UserResponse{..} =
  UserRequest {
    userRequestName = userRequestName,
    userRequestEmail = userRequestEmail
  }

-- footer
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Convert where




import           Data.Aeson  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text   (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))

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
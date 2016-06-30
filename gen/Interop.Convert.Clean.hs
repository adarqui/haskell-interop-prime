{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop.Convert.Clean where




import Data.Aeson
import Data.Text   (Text)
import Data.Monoid ((<>))

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
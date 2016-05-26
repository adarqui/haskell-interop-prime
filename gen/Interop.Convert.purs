module Interop.Convert where


import Control.Monad.Aff
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Printer
import Data.Date.Helpers
import Data.Either
import Data.Foreign
import Data.Foreign.NullOrUndefined
import Data.Foreign.Class
import Data.JSON
import Data.List (List ())
import Data.Maybe
import Data.Set (Set ())
import Data.Tuple
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Optic.Lens
import Optic.Core
import Prelude

userRequestToUserResponse :: Int -> String -> String -> Boolean -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse userResponseId userResponseName userResponseEmail userResponseActive userResponseCreatedAt userResponseModifiedAt (UserRequest o) =
  UserResponse {
    userResponseId: userResponseId,
    userResponseName: userResponseName,
    userResponseEmail: userResponseEmail,
    userResponseActive: userResponseActive,
    userResponseCreatedAt: userResponseCreatedAt,
    userResponseModifiedAt: userResponseModifiedAt
  }


userResponseToUserRequest :: String -> String -> UserResponse -> UserRequest
userResponseToUserRequest userRequestName userRequestEmail (UserResponse o) =
  UserRequest {
    userRequestName: userRequestName,
    userRequestEmail: userRequestEmail
  }



-- footer
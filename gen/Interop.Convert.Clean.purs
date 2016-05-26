module Interop.Convert.Clean where


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

userRequestToUserResponse :: Int -> Boolean -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse id active createdAt modifiedAt (UserRequest o) =
  UserResponse {
    id: id,
    name: o.name,
    email: o.email,
    active: active,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  (UserResponse o) =
  UserRequest {
    name: o.name,
    email: o.email
  }



-- footer
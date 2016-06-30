module Interop.Convert.Clean where


import Control.Monad.Aff                ()
import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date(..))
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..))
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class IsForeign, read, readProp)
import Data.List                        (List ())
import Data.Maybe                       (Maybe(..))
import Data.Set                         (Set ())
import Data.Tuple                       (Tuple(..))
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==))

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
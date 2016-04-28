module Interop.Api where


import Purescript.Api.Helpers
import Prelude
import Data.Either
import Data.Tuple

getUsers :: forall qp. QueryParam qp => Array qp -> ApiEff (Either ApiError UserResponses)
getUsers params = handleError <$> getAt params ["users"]

getUsers' :: ApiEff (Either ApiError UserResponses)
getUsers'  = handleError <$> getAt ([] :: Array Boolean) ["users"]

getUsers_UsersIds :: forall qp. QueryParam qp => Array qp -> (Array  Int) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersIds params _UsersIds = handleError <$> getAt (map qp params ++ map qp [UsersIds _UsersIds]) ["users"]

getUsers_UsersIds' :: (Array  Int) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersIds' _UsersIds = handleError <$> getAt [UsersIds _UsersIds] ["users"]

getUsers_UsersNames :: forall qp. QueryParam qp => Array qp -> (Array  String) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersNames params _UsersNames = handleError <$> getAt (map qp params ++ map qp [UsersNames _UsersNames]) ["users"]

getUsers_UsersNames' :: (Array  String) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersNames' _UsersNames = handleError <$> getAt [UsersNames _UsersNames] ["users"]

getUsers_UsersEmails :: forall qp. QueryParam qp => Array qp -> (Array  String) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersEmails params _UsersEmails = handleError <$> getAt (map qp params ++ map qp [UsersEmails _UsersEmails]) ["users"]

getUsers_UsersEmails' :: (Array  String) -> ApiEff (Either ApiError UserResponses)
getUsers_UsersEmails' _UsersEmails = handleError <$> getAt [UsersEmails _UsersEmails] ["users"]

postUser :: forall qp. QueryParam qp => Array qp -> UserRequest -> ApiEff (Either ApiError UserResponse)
postUser params user_request = handleError <$> postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff (Either ApiError UserResponse)
postUser' user_request = handleError <$> postAt ([] :: Array Boolean) ["user"] user_request

getUser :: forall qp. QueryParam qp => Array qp -> Int -> ApiEff (Either ApiError UserResponse)
getUser params user_id = handleError <$> getAt params ["user", show user_id]

getUser' :: Int -> ApiEff (Either ApiError UserResponse)
getUser' user_id = handleError <$> getAt ([] :: Array Boolean) ["user", show user_id]

putUser :: forall qp. QueryParam qp => Array qp -> Int -> UserRequest -> ApiEff (Either ApiError UserResponse)
putUser params user_id user_request = handleError <$> putAt params ["user", show user_id] user_request

putUser' :: Int -> UserRequest -> ApiEff (Either ApiError UserResponse)
putUser' user_id user_request = handleError <$> putAt ([] :: Array Boolean) ["user", show user_id] user_request

deleteUser :: forall qp. QueryParam qp => Array qp -> Int -> ApiEff (Either ApiError Unit)
deleteUser params user_id = handleError <$> deleteAt params ["user", show user_id]

deleteUser' :: Int -> ApiEff (Either ApiError Unit)
deleteUser' user_id = handleError <$> deleteAt ([] :: Array Boolean) ["user", show user_id]

-- footer
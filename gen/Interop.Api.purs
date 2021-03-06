module Interop.Api where


import Data.Either            (Either)
import Prelude                (Unit, show, map, (<$>), (<>))
import Purescript.Api.Helpers (class QueryParam, ApiError, ApiEff, getAt, handleError, qp, deleteAt, putAt, postAt)
import Data.Default

getUsers :: forall qp. QueryParam qp => Array qp -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers params = handleError <$> getAt params ["users"]

getUsers' :: ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers'  = handleError <$> getAt ([] :: Array Boolean) ["users"]

getUsers_ByUsersIds :: forall qp. QueryParam qp => Array qp -> (Array Int) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: (Array Int) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: forall qp. QueryParam qp => Array qp -> (Array String) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames params _ByUsersNames = handleError <$> getAt (map qp params <> map qp [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: (Array String) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames' _ByUsersNames = handleError <$> getAt [ByUsersNames _ByUsersNames] ["users"]

getUsers_ByUsersEmails :: forall qp. QueryParam qp => Array qp -> (Array String) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersEmails params _ByUsersEmails = handleError <$> getAt (map qp params <> map qp [ByUsersEmails _ByUsersEmails]) ["users"]

getUsers_ByUsersEmails' :: (Array String) -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersEmails' _ByUsersEmails = handleError <$> getAt [ByUsersEmails _ByUsersEmails] ["users"]

getUser_ByUserActive :: forall qp. QueryParam qp => Array qp -> Int -> Boolean -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser_ByUserActive params user_id _ByUserActive = handleError <$> getAt (map qp params <> map qp [ByUserActive _ByUserActive]) ["user", show user_id]

getUser_ByUserActive' :: Int -> Boolean -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser_ByUserActive' user_id _ByUserActive = handleError <$> getAt [ByUserActive _ByUserActive] ["user", show user_id]

postUser :: forall qp. QueryParam qp => Array qp -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUser params user_request = handleError <$> postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
postUser' user_request = handleError <$> postAt ([] :: Array Boolean) ["user"] user_request

getUser :: forall qp. QueryParam qp => Array qp -> Int -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser params user_id = handleError <$> getAt params ["user", show user_id]

getUser' :: Int -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUser' user_id = handleError <$> getAt ([] :: Array Boolean) ["user", show user_id]

putUser :: forall qp. QueryParam qp => Array qp -> Int -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUser params user_id user_request = handleError <$> putAt params ["user", show user_id] user_request

putUser' :: Int -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUser' user_id user_request = handleError <$> putAt ([] :: Array Boolean) ["user", show user_id] user_request

deleteUser :: forall qp. QueryParam qp => Array qp -> Int -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUser params user_id = handleError <$> deleteAt params ["user", show user_id]

deleteUser' :: Int -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUser' user_id = handleError <$> deleteAt ([] :: Array Boolean) ["user", show user_id]

getUserText :: forall qp. QueryParam qp => Array qp -> String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUserText params user_name = handleError <$> getAt params ["user_text", user_name]

getUserText' :: String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUserText' user_name = handleError <$> getAt ([] :: Array Boolean) ["user_text", user_name]

putUserText :: forall qp. QueryParam qp => Array qp -> String -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUserText params user_name user_request = handleError <$> putAt params ["user_text", user_name] user_request

putUserText' :: String -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUserText' user_name user_request = handleError <$> putAt ([] :: Array Boolean) ["user_text", user_name] user_request

deleteUserText :: forall qp. QueryParam qp => Array qp -> String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUserText params user_name = handleError <$> deleteAt params ["user_text", user_name]

deleteUserText' :: String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUserText' user_name = handleError <$> deleteAt ([] :: Array Boolean) ["user_text", user_name]

getUserString :: forall qp. QueryParam qp => Array qp -> String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUserString params user_name = handleError <$> getAt params ["user_string", user_name]

getUserString' :: String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
getUserString' user_name = handleError <$> getAt ([] :: Array Boolean) ["user_string", user_name]

putUserString :: forall qp. QueryParam qp => Array qp -> String -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUserString params user_name user_request = handleError <$> putAt params ["user_string", user_name] user_request

putUserString' :: String -> UserRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) UserResponse)
putUserString' user_name user_request = handleError <$> putAt ([] :: Array Boolean) ["user_string", user_name] user_request

deleteUserString :: forall qp. QueryParam qp => Array qp -> String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUserString params user_name = handleError <$> deleteAt params ["user_string", user_name]

deleteUserString' :: String -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) Unit)
deleteUserString' user_name = handleError <$> deleteAt ([] :: Array Boolean) ["user_string", user_name]

-- footer
module Interop.Api where


import Purescript.Api.Helpers

getUsers :: Array (Tuple String String) -> ApiEff (Either ForeignError UserResponses)
getUsers params = getAt params ["users"]

getUsers' :: ApiEff (Either ForeignError UserResponses)
getUsers'  = getAt  ["users"]

getUsers_ByUsersIds :: Array (Tuple String String) -> (Array  Int) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersIds params _ByUsersIds = getAt (params ++ [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: (Array  Int) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersIds' _ByUsersIds = getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: Array (Tuple String String) -> (Array  String) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersNames params _ByUsersNames = getAt (params ++ [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: (Array  String) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersNames' _ByUsersNames = getAt [ByUsersNames _ByUsersNames] ["users"]

getUsers_ByUsersEmails :: Array (Tuple String String) -> (Array  String) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersEmails params _ByUsersEmails = getAt (params ++ [ByUsersEmails _ByUsersEmails]) ["users"]

getUsers_ByUsersEmails' :: (Array  String) -> ApiEff (Either ForeignError UserResponses)
getUsers_ByUsersEmails' _ByUsersEmails = getAt [ByUsersEmails _ByUsersEmails] ["users"]

postUser :: Array (Tuple String String) -> UserRequest -> ApiEff (Either ForeignError UserResponse)
postUser params user_request = postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff (Either ForeignError UserResponse)
postUser' user_request = postAt  ["user"] user_request

getUser :: Array (Tuple String String) -> Int -> ApiEff (Either ForeignError UserResponse)
getUser params user_id = getAt params ["user", show user_id]

getUser' :: Int -> ApiEff (Either ForeignError UserResponse)
getUser' user_id = getAt  ["user", show user_id]

putUser :: Array (Tuple String String) -> Int -> UserRequest -> ApiEff (Either ForeignError UserResponse)
putUser params user_id user_request = putAt params ["user", show user_id] user_request

putUser' :: Int -> UserRequest -> ApiEff (Either ForeignError UserResponse)
putUser' user_id user_request = putAt  ["user", show user_id] user_request

deleteUser :: Array (Tuple String String) -> Int -> ApiEff (Either ForeignError Unit)
deleteUser params user_id = deleteAt params ["user", show user_id]

deleteUser' :: Int -> ApiEff (Either ForeignError Unit)
deleteUser' user_id = deleteAt  ["user", show user_id]

-- footer
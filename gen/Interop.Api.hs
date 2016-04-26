{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Interop.Api where




import Haskell.Api.Helpers

default (Text)


getUsers :: [(String, String)] -> ApiEff (Either Status UserResponses)
getUsers params = getAt params ["users"]

getUsers' :: ApiEff (Either Status UserResponses)
getUsers'  = getAt [] ["users"]

getUsers_ByUsersIds :: [(String, String)] -> [Int64] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersIds params _ByUsersIds = getAt (params ++ [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: [Int64] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersIds' _ByUsersIds = getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: [(String, String)] -> [String] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersNames params _ByUsersNames = getAt (params ++ [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: [String] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersNames' _ByUsersNames = getAt [ByUsersNames _ByUsersNames] ["users"]

getUsers_ByUsersEmails :: [(String, String)] -> [String] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersEmails params _ByUsersEmails = getAt (params ++ [ByUsersEmails _ByUsersEmails]) ["users"]

getUsers_ByUsersEmails' :: [String] -> ApiEff (Either Status UserResponses)
getUsers_ByUsersEmails' _ByUsersEmails = getAt [ByUsersEmails _ByUsersEmails] ["users"]

postUser :: [(String, String)] -> UserRequest -> ApiEff (Either Status UserResponse)
postUser params user_request = postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff (Either Status UserResponse)
postUser' user_request = postAt [] ["user"] user_request

getUser :: [(String, String)] -> Int64 -> ApiEff (Either Status UserResponse)
getUser params user_id = getAt params ["user", show user_id]

getUser' :: Int64 -> ApiEff (Either Status UserResponse)
getUser' user_id = getAt [] ["user", show user_id]

putUser :: [(String, String)] -> Int64 -> UserRequest -> ApiEff (Either Status UserResponse)
putUser params user_id user_request = putAt params ["user", show user_id] user_request

putUser' :: Int64 -> UserRequest -> ApiEff (Either Status UserResponse)
putUser' user_id user_request = putAt [] ["user", show user_id] user_request

deleteUser :: [(String, String)] -> Int64 -> ApiEff (Either Status ())
deleteUser params user_id = deleteAt params ["user", show user_id]

deleteUser' :: Int64 -> ApiEff (Either Status ())
deleteUser' user_id = deleteAt [] ["user", show user_id]

-- footer
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop.Api where




import Haskell.Api.Helpers
import Data.Int

getUsers :: forall qp. QueryParam qp => [qp] -> ApiEff (Either ApiError UserResponses)
getUsers params = handleError <$> getAt params ["users"]

getUsers' :: ApiEff (Either ApiError UserResponses)
getUsers'  = handleError <$> getAt ([] :: [(String, String)]) ["users"]

getUsers_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params ++ map qp [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: [Int64] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersNames params _ByUsersNames = handleError <$> getAt (map qp params ++ map qp [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: [String] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersNames' _ByUsersNames = handleError <$> getAt [ByUsersNames _ByUsersNames] ["users"]

getUsers_ByUsersEmails :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersEmails params _ByUsersEmails = handleError <$> getAt (map qp params ++ map qp [ByUsersEmails _ByUsersEmails]) ["users"]

getUsers_ByUsersEmails' :: [String] -> ApiEff (Either ApiError UserResponses)
getUsers_ByUsersEmails' _ByUsersEmails = handleError <$> getAt [ByUsersEmails _ByUsersEmails] ["users"]

getUser_ByUserActive :: forall qp. QueryParam qp => [qp] -> Int64 -> Bool -> ApiEff (Either ApiError UserResponse)
getUser_ByUserActive params user_id _ByUserActive = handleError <$> getAt (map qp params ++ map qp [ByUserActive _ByUserActive]) ["user", show user_id]

getUser_ByUserActive' :: Int64 -> Bool -> ApiEff (Either ApiError UserResponse)
getUser_ByUserActive' user_id _ByUserActive = handleError <$> getAt [ByUserActive _ByUserActive] ["user", show user_id]

postUser :: forall qp. QueryParam qp => [qp] -> UserRequest -> ApiEff (Either ApiError UserResponse)
postUser params user_request = handleError <$> postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff (Either ApiError UserResponse)
postUser' user_request = handleError <$> postAt ([] :: [(String, String)]) ["user"] user_request

getUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff (Either ApiError UserResponse)
getUser params user_id = handleError <$> getAt params ["user", show user_id]

getUser' :: Int64 -> ApiEff (Either ApiError UserResponse)
getUser' user_id = handleError <$> getAt ([] :: [(String, String)]) ["user", show user_id]

putUser :: forall qp. QueryParam qp => [qp] -> Int64 -> UserRequest -> ApiEff (Either ApiError UserResponse)
putUser params user_id user_request = handleError <$> putAt params ["user", show user_id] user_request

putUser' :: Int64 -> UserRequest -> ApiEff (Either ApiError UserResponse)
putUser' user_id user_request = handleError <$> putAt ([] :: [(String, String)]) ["user", show user_id] user_request

deleteUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff (Either ApiError ())
deleteUser params user_id = handleError <$> deleteAt params ["user", show user_id]

deleteUser' :: Int64 -> ApiEff (Either ApiError ())
deleteUser' user_id = handleError <$> deleteAt ([] :: [(String, String)]) ["user", show user_id]

-- footer
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

getUsers_UsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersIds params _UsersIds = handleError <$> getAt (map qp params ++ map qp [UsersIds _UsersIds]) ["users"]

getUsers_UsersIds' :: [Int64] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersIds' _UsersIds = handleError <$> getAt [UsersIds _UsersIds] ["users"]

getUsers_UsersNames :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersNames params _UsersNames = handleError <$> getAt (map qp params ++ map qp [UsersNames _UsersNames]) ["users"]

getUsers_UsersNames' :: [String] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersNames' _UsersNames = handleError <$> getAt [UsersNames _UsersNames] ["users"]

getUsers_UsersEmails :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersEmails params _UsersEmails = handleError <$> getAt (map qp params ++ map qp [UsersEmails _UsersEmails]) ["users"]

getUsers_UsersEmails' :: [String] -> ApiEff (Either ApiError UserResponses)
getUsers_UsersEmails' _UsersEmails = handleError <$> getAt [UsersEmails _UsersEmails] ["users"]

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
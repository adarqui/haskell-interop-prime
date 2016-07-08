{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Api where




import Data.Int            (Int64)
import Data.Monoid         ((<>))
import Data.Text           (Text)
import qualified Data.Text as T (pack)
import Haskell.Api.Helpers (ApiEff, ApiError, QueryParam, qp, handleError, getAt, putAt, postAt, deleteAt)
import Data.Default

getUsers :: forall qp. QueryParam qp => [qp] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers params = handleError <$> getAt params ["users"]

getUsers' :: ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers'  = handleError <$> getAt ([] :: [(Text, Text)]) ["users"]

getUsers_ByUsersIds :: forall qp. QueryParam qp => [qp] -> [Int64] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds params _ByUsersIds = handleError <$> getAt (map qp params <> map qp [ByUsersIds _ByUsersIds]) ["users"]

getUsers_ByUsersIds' :: [Int64] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersIds' _ByUsersIds = handleError <$> getAt [ByUsersIds _ByUsersIds] ["users"]

getUsers_ByUsersNames :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames params _ByUsersNames = handleError <$> getAt (map qp params <> map qp [ByUsersNames _ByUsersNames]) ["users"]

getUsers_ByUsersNames' :: [String] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersNames' _ByUsersNames = handleError <$> getAt [ByUsersNames _ByUsersNames] ["users"]

getUsers_ByUsersEmails :: forall qp. QueryParam qp => [qp] -> [String] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersEmails params _ByUsersEmails = handleError <$> getAt (map qp params <> map qp [ByUsersEmails _ByUsersEmails]) ["users"]

getUsers_ByUsersEmails' :: [String] -> ApiEff (Either (ApiError ApplicationError) UserResponses)
getUsers_ByUsersEmails' _ByUsersEmails = handleError <$> getAt [ByUsersEmails _ByUsersEmails] ["users"]

getUser_ByUserActive :: forall qp. QueryParam qp => [qp] -> Int64 -> Bool -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUser_ByUserActive params user_id _ByUserActive = handleError <$> getAt (map qp params <> map qp [ByUserActive _ByUserActive]) ["user", T.pack $ show user_id]

getUser_ByUserActive' :: Int64 -> Bool -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUser_ByUserActive' user_id _ByUserActive = handleError <$> getAt [ByUserActive _ByUserActive] ["user", T.pack $ show user_id]

postUser :: forall qp. QueryParam qp => [qp] -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
postUser params user_request = handleError <$> postAt params ["user"] user_request

postUser' :: UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
postUser' user_request = handleError <$> postAt ([] :: [(Text, Text)]) ["user"] user_request

getUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUser params user_id = handleError <$> getAt params ["user", T.pack $ show user_id]

getUser' :: Int64 -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUser' user_id = handleError <$> getAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id]

putUser :: forall qp. QueryParam qp => [qp] -> Int64 -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUser params user_id user_request = handleError <$> putAt params ["user", T.pack $ show user_id] user_request

putUser' :: Int64 -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUser' user_id user_request = handleError <$> putAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id] user_request

deleteUser :: forall qp. QueryParam qp => [qp] -> Int64 -> ApiEff (Either (ApiError ApplicationError) ())
deleteUser params user_id = handleError <$> deleteAt params ["user", T.pack $ show user_id]

deleteUser' :: Int64 -> ApiEff (Either (ApiError ApplicationError) ())
deleteUser' user_id = handleError <$> deleteAt ([] :: [(Text, Text)]) ["user", T.pack $ show user_id]

getUserText :: forall qp. QueryParam qp => [qp] -> String -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUserText params user_name = handleError <$> getAt params ["user_text", user_name]

getUserText' :: String -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUserText' user_name = handleError <$> getAt ([] :: [(Text, Text)]) ["user_text", user_name]

putUserText :: forall qp. QueryParam qp => [qp] -> String -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUserText params user_name user_request = handleError <$> putAt params ["user_text", user_name] user_request

putUserText' :: String -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUserText' user_name user_request = handleError <$> putAt ([] :: [(Text, Text)]) ["user_text", user_name] user_request

deleteUserText :: forall qp. QueryParam qp => [qp] -> String -> ApiEff (Either (ApiError ApplicationError) ())
deleteUserText params user_name = handleError <$> deleteAt params ["user_text", user_name]

deleteUserText' :: String -> ApiEff (Either (ApiError ApplicationError) ())
deleteUserText' user_name = handleError <$> deleteAt ([] :: [(Text, Text)]) ["user_text", user_name]

getUserString :: forall qp. QueryParam qp => [qp] -> [Char] -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUserString params user_name = handleError <$> getAt params ["user_string", T.pack $ show user_name]

getUserString' :: [Char] -> ApiEff (Either (ApiError ApplicationError) UserResponse)
getUserString' user_name = handleError <$> getAt ([] :: [(Text, Text)]) ["user_string", T.pack $ show user_name]

putUserString :: forall qp. QueryParam qp => [qp] -> [Char] -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUserString params user_name user_request = handleError <$> putAt params ["user_string", T.pack $ show user_name] user_request

putUserString' :: [Char] -> UserRequest -> ApiEff (Either (ApiError ApplicationError) UserResponse)
putUserString' user_name user_request = handleError <$> putAt ([] :: [(Text, Text)]) ["user_string", T.pack $ show user_name] user_request

deleteUserString :: forall qp. QueryParam qp => [qp] -> [Char] -> ApiEff (Either (ApiError ApplicationError) ())
deleteUserString params user_name = handleError <$> deleteAt params ["user_string", T.pack $ show user_name]

deleteUserString' :: [Char] -> ApiEff (Either (ApiError ApplicationError) ())
deleteUserString' user_name = handleError <$> deleteAt ([] :: [(Text, Text)]) ["user_string", T.pack $ show user_name]

-- footer
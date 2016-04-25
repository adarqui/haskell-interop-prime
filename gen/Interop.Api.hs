getUsers :: UserResponses
getUsers  = undefined

getUsers_ByUsersIds :: [Int64] -> UserResponses
getUsers_ByUsersIds _ByUsersIds = undefined

getUsers_ByUsersNames :: [String] -> UserResponses
getUsers_ByUsersNames _ByUsersNames = undefined

getUsers_ByUsersEmails :: [String] -> UserResponses
getUsers_ByUsersEmails _ByUsersEmails = undefined

postUser :: UserRequest -> UserResponse
postUser user_request = undefined

getUser :: Int64 -> UserResponse
getUser user_id = undefined

putUser :: Int64 -> UserRequest -> UserResponse
putUser user_id user_request = undefined

deleteUser :: Int64 -> ()
deleteUser user_id = undefined

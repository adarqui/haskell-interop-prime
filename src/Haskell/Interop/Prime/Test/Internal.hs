module Haskell.Interop.Prime.Test.Internal (
  apiSpec,
  apiEntries
) where



import           Haskell.Interop.Prime



apiSpec :: Api
apiSpec = Api {
  apiPrefix  = "/api",
  apiEntries = apiEntries'
}


apiEntries' :: [ApiEntry]
apiEntries' =
  [
    -- BigRecords
    --
    ApiEntry "BigRecords"
    [ ParNone
    , ParBy "BigRecordsIds" "[Int64]"
    , ParBy "BigRecordsStrings" "[String]"
    ]
    [ ApiGET "BigRecordResponses" ]

  , ApiEntry "BigRecord"
    [ ParNone ]
    [ ApiPOST "BigRecordRequest" "BigRecordResponse"]

  , ApiEntry "BigRecord"
    [ Par [("record_id", "Int64")]]
    [ ApiGET "BigRecordResponse"
    , ApiPUT "BigRecordRequest" "BigRecordResponse"
    , ApiDELETE "()"]

  -- Users
  --
  , ApiEntry "Users"
    [ ParNone
    , ParBy "UsersIds" "[Int64]"
    , ParBy "UsersNames" "[String]"
    , ParBy "UsersEmails" "[String]"
    ]
    [ ApiGET "UserResponses" ]

  , ApiEntry "User"
    [ ParNone ]
    [ ApiPOST "UserRequest" "UserResponse" ]

  , ApiEntry "User"
    [ Par [("user_id", "Int64")] ]
    [ ApiGET "UserResponse"
    , ApiPUT "UserRequest" "UserResponse"
    , ApiDELETE "()"
    ]
  ]

{-# LANGUAGE TemplateHaskell #-}

module Haskell.Interop.Prime.Test.Internal (
  apiSpec_TH,
  apiEntries_TH
) where



import           Data.Int
import           Haskell.Interop.Prime
import           Haskell.Interop.Prime.Test.Types



type Int64_L = [Int64]
type String_L = [String]



apiSpec_TH :: Api_TH
apiSpec_TH = Api_TH {
  apiPrefix_TH = "/api",
  apiEntries_TH = apiEntries_TH'
}


apiEntries_TH' :: [ApiEntry_TH]
apiEntries_TH' =
  [ ApiEntry_TH "Users"
    [ ParNone_TH
    , ParBy_TH "UsersIds" ''Int64_L
    , ParBy_TH "UsersNames" ''String_L
    , ParBy_TH "UsersEmails" ''String_L
    ]
    [ ApiGET_TH ''UserResponses ]

  , ApiEntry_TH "User"
    [ ParNone_TH ]
    [ ApiPOST_TH ''UserRequest ''UserResponse ]

  , ApiEntry_TH "User"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserResponse
    , ApiPUT_TH ''UserRequest ''UserResponse
    , ApiDELETE_TH ''()
    ]
  ]

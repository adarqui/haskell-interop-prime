{-# LANGUAGE TemplateHaskell #-}

module Haskell.Interop.Prime.Test.Internal (
  apiSpec_TH,
  apiEntries_TH,
  psMksQueryParam,
  hsMksQueryParam
) where



import           Data.Int
import           Data.Monoid                      ((<>))
import           Prelude

import           Haskell.Interop.Prime
import           Haskell.Interop.Prime.Test.Types



type Int64_L = [Int64]
type String_L = [String]



psMksQueryParam :: [Mk]
psMksQueryParam = defaultPurescriptMks <> [MkQueryParam]



hsMksQueryParam :: [Mk]
hsMksQueryParam = defaultHaskellMks <> [MkQueryParam]



apiSpec_TH :: Api_TH
apiSpec_TH = Api_TH {
  apiPrefix_TH = "/api",
  apiEntries_TH = apiEntries_TH'
}



apiEntries_TH' :: [ApiEntry_TH]
apiEntries_TH' =
  [ ApiEntry_TH "Users"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUsersNames" ''String_L
    , ParBy_TH "ByUsersEmails" ''String_L
    ]
    [ ApiGET_TH ''UserResponses ]

  , ApiEntry_TH "User"
    [ ParBoth_TH [("user_id", ''Int64)] ("ByUserActive", ''Bool) ]
    [ ApiGET_TH ''UserResponse ]

  , ApiEntry_TH "User"
    [ ParNone_TH ]
    [ ApiPOST_TH ''UserRequest ''UserResponse ]

  , ApiEntry_TH "User"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserResponse
    , ApiPUT_TH ''UserRequest ''UserResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "UserText"
    [ Par_TH [("user_name", ''Text)] ]
    [ ApiGET_TH ''UserResponse
    , ApiPUT_TH ''UserRequest ''UserResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "UserString"
    [ Par_TH [("user_name", ''String)] ]
    [ ApiGET_TH ''UserResponse
    , ApiPUT_TH ''UserRequest ''UserResponse
    , ApiDELETE_TH ''()
    ]
  ]

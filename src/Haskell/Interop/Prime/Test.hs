{-# LANGUAGE TemplateHaskell #-}

module Haskell.Interop.Prime.Test where



import           Data.Int
import           Haskell.Interop.Prime
import           Haskell.Interop.Prime.Test.Internal



newtype Session = Session { unSession :: String }

data SumType
  = A
  | B Int
  | C Bool
  | D String
  | E [Int]
  | F SumType
  | G [SumType]
  | H Bool Int String (Maybe Bool)

data BigRecord = BigRecord {
  bigRecordBool         :: Bool,
  bigRecordInt          :: Int,
  bigRecordMaybeInt     :: Maybe Int,
  bigRecordInteger      :: Integer,
  bigRecordMaybeInteger :: Maybe Integer,
  bigRecordString       :: String,
  bigRecordSumType      :: SumType,
  bigRecordData         :: String,
  bigRecordClass        :: String,
  bigRecordLet          :: String,
  bigRecordModule       :: String,
  bigRecord             :: Bool
}

data User = User {
  userName  :: String,
  userEmail :: String
}

data UserRequest = UserRequest {
  userRequestName  :: String,
  userRequestEmail :: String
}

type FakeUTCTime = Integer

data UserResponse = UserResponse {
  userResponseId         :: Int64,
  userResponseName       :: String,
  userResponseEmail      :: String,
  userResponseCreatedAt  :: Maybe FakeUTCTime,
  userResponseModifiedAt :: Maybe FakeUTCTime
}

data UserResponses = UserResponses {
  userResponses :: [UserResponse]
}

newtype DateMaybe = DateMaybe (Maybe String)

type Text = String

type TextMaybe = Maybe Text

data FunkyRecord
  = Boom1 { boom1 :: Bool }
  | Boom2 { boom2 :: Bool }
  | Boom3 { boom3a :: Bool, boom3b :: Bool, boom3c :: Bool }
  | Boom4



mkExports
  (Options
    (defaultOptionsPurescript "/tmp/Interop.purs")
    (defaultPurescriptMkGs "module Interop where")
    (defaultOptionsHaskell "/tmp/Interop.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop"))
  [ (''Session,      defaultPurescriptMks, defaultHaskellMks)
  , (''SumType,      defaultPurescriptMks, defaultHaskellMks)
  , (''BigRecord,    defaultPurescriptMks, defaultHaskellMks)
  , (''FakeUTCTime,  defaultPurescriptMks, defaultHaskellMks)
  , (''User,         defaultPurescriptMks, defaultHaskellMks)
  , (''UserRequest,  defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponse, defaultPurescriptMks, defaultHaskellMks)
  , (''DateMaybe,    defaultPurescriptMks, defaultHaskellMks)
  , (''Text,         defaultPurescriptMks, defaultHaskellMks)
  , (''TextMaybe,    defaultPurescriptMks, defaultHaskellMks)
  , (''FunkyRecord,  defaultPurescriptMks, defaultHaskellMks)
  ]



mkExports
  (Options
    (defaultOptionsCleanPurescript "/tmp/Interop.Clean.purs")
    (defaultPurescriptMkGs "module Interop.Clean where")
    (defaultOptionsCleanHaskell "/tmp/Interop.Clean.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop.Clean"))
  [ (''Session,      defaultPurescriptMks, defaultHaskellMks)
  , (''SumType,      defaultPurescriptMks, defaultHaskellMks)
  , (''BigRecord,    defaultPurescriptMks, defaultHaskellMks)
  , (''FakeUTCTime,  defaultPurescriptMks, defaultHaskellMks)
  , (''User,         defaultPurescriptMks, defaultHaskellMks)
  , (''UserRequest,  defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponse, defaultPurescriptMks, defaultHaskellMks)
  , (''DateMaybe,    defaultPurescriptMks, defaultHaskellMks)
  , (''Text,         defaultPurescriptMks, defaultHaskellMks)
  , (''TextMaybe,    defaultPurescriptMks, defaultHaskellMks)
  , (''FunkyRecord,  defaultPurescriptMks, defaultHaskellMks)
  ]



mkApi
  (Options
    (defaultOptionsCleanPurescript "/tmp/Interop.Api.purs")
    (defaultPurescriptMkGs "module Interop.Api where")
    (defaultOptionsCleanHaskell "/tmp/Interop.Api.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop.Api"))
  apiSpec

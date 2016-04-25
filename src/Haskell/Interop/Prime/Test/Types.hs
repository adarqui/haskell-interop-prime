module Haskell.Interop.Prime.Test.Types (
  Session (..),
  SumType (..),
  BigRecord (..),
  User (..),
  UserRequest (..),
  FakeUTCTime,
  UserResponse (..),
  UserResponses (..),
  DateMaybe (..),
  Text,
  TextMaybe,
  FunkyRecord (..)
) where



import           Data.Int



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

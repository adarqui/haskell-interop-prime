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
  NestedList,
  FunkyRecord (..),
  FUnkyRecordP (..),
  Param (..),
  ParamTag (..),
  ApplicationError (..)
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
  bigRecordString2      :: [Char],
  bigRecordSumType      :: SumType,
  bigRecordData         :: String,
  bigRecordClass        :: String,
  bigRecordLet          :: String,
  bigRecordModule       :: String,
  bigRecord             :: Bool
}



data User = User {
  userName   :: String,
  userEmail  :: String,
  userActive :: Bool
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
  userResponseActive     :: Bool,
  userResponseCreatedAt  :: Maybe FakeUTCTime,
  userResponseModifiedAt :: Maybe FakeUTCTime
}



data UserResponses = UserResponses {
  userResponses :: [UserResponse]
}



newtype DateMaybe = DateMaybe (Maybe String)



type Text = String



type TextMaybe = Maybe Text



type NestedList a = [[a]]



data FunkyRecord
  = Boom1 { boom1 :: Bool }
  | Boom2 { boom2 :: Bool }
  | Boom3 { boom3a :: Bool, boom3b :: Bool, boom3c :: Bool }
  | Boom4



data FUnkyRecordP = FUnkyRecordP {
  funkyrecordpField :: Bool
}



data Param
  = Limit          Int
  | Offset         Int
  | ByUsersIds     [Int64]
  | ByUserNameText Text
  | ByUserNameStr  String
  | ByUsersNames   [String]
  | ByUsersEmails  [String]
  | ByUserActive   Bool



data ParamTag
  = ParamTag_Limit
  | ParamTag_Offset
  | ParamTag_ByUsersIds
  | ParamTag_ByUsersNames
  | ParamTag_ByUsersEmails
  | ParamTag_ByUserActive



-- Ripped from my haskell-api-helpers library to give an idea of how the query params work
--
class QueryParam a where
  qp :: a -> (String, String)



-- Used by haskell-api-helpers to return JSON errors: FromJSON a, FromJSON b, Default b => Either (ApiError b) a
--
data ApplicationError
  = Error_Unknown
  | Error_Validation
  | Error_PerimssionDenied

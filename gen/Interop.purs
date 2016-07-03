module Interop where


import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..))
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class IsForeign, read, readProp)
import Data.Maybe                       (Maybe(..))
import Data.Tuple                       (Tuple(..))
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, pure, bind, ($), (<>), (<$>), (<*>), (==))

newtype Session = Session {
  unSession :: String
}


type SessionR = {
  unSession :: String
}


_Session :: Lens' Session {
  unSession :: String
}
_Session f (Session o) = Session <$> f o


mkSession :: String -> Session
mkSession unSession =
  Session{unSession}


unwrapSession :: Session -> {
  unSession :: String
}
unwrapSession (Session r) = r

instance sessionEncodeJson :: EncodeJson Session where
  encodeJson (Session o) =
       "tag" := "Session"
    ~> "unSession" := o.unSession
    ~> jsonEmptyObject


instance sessionDecodeJson :: DecodeJson Session where
  decodeJson o = do
    obj <- decodeJson o
    unSession <- obj .? "unSession"
    pure $ Session {
      unSession
    }


instance sessionRequestable :: Requestable Session where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sessionRespondable :: Respondable Session where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSession
      <$> readProp "unSession" json


instance sessionIsForeign :: IsForeign Session where
  read json =
      mkSession
      <$> readProp "unSession" json


instance sessionShow :: Show Session where
    show (Session o) = show "unSession: " <> show o.unSession

instance sessionEq :: Eq Session where
  eq (Session a) (Session b) = a.unSession == b.unSession

data SumType
  = A 
  | B Int
  | C Boolean
  | D String
  | E (Array Int)
  | F SumType
  | G (Array SumType)
  | H Boolean Int String (Maybe Boolean)



instance sumTypeEncodeJson :: EncodeJson SumType where
  encodeJson (A ) =
       "tag" := "A"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (B x0) =
       "tag" := "B"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (C x0) =
       "tag" := "C"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (D x0) =
       "tag" := "D"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (E x0) =
       "tag" := "E"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (F x0) =
       "tag" := "F"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (G x0) =
       "tag" := "G"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (H x0 x1 x2 x3) =
       "tag" := "H"
    ~> "contents" := [encodeJson x0, encodeJson x1, encodeJson x2, encodeJson x3]
    ~> jsonEmptyObject


instance sumTypeDecodeJson :: DecodeJson SumType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "A" -> do
        pure A

      "B" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> B <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for B"


      "C" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> C <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for C"


      "D" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> D <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for D"


      "E" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> E <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for E"


      "F" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> F <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for F"


      "G" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> G <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for G"


      "H" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1, x2, x3] -> H <$> decodeJson x0 <*> decodeJson x1 <*> decodeJson x2 <*> decodeJson x3
          _ -> Left $ "DecodeJson TypeMismatch for H"


      _ -> Left $ "DecodeJson TypeMismatch for SumType"



instance sumTypeRequestable :: Requestable SumType where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sumTypeRespondable :: Respondable SumType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "A" -> do
        pure A

      "B" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> B <$> read x0
          _ -> Left $ TypeMismatch "B" "Respondable"


      "C" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> C <$> read x0
          _ -> Left $ TypeMismatch "C" "Respondable"


      "D" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> D <$> read x0
          _ -> Left $ TypeMismatch "D" "Respondable"


      "E" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> E <$> read x0
          _ -> Left $ TypeMismatch "E" "Respondable"


      "F" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> F <$> read x0
          _ -> Left $ TypeMismatch "F" "Respondable"


      "G" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> G <$> read x0
          _ -> Left $ TypeMismatch "G" "Respondable"


      "H" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1, x2, x3] -> H <$> read x0 <*> read x1 <*> read x2 <*> read x3
          _ -> Left $ TypeMismatch "H" "Respondable"


      _ -> Left $ TypeMismatch "SumType" "Respondable"



instance sumTypeIsForeign :: IsForeign SumType where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "A" -> do
        pure A

      "B" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> B <$> read x0
          _ -> Left $ TypeMismatch "B" "IsForeign"


      "C" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> C <$> read x0
          _ -> Left $ TypeMismatch "C" "IsForeign"


      "D" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> D <$> read x0
          _ -> Left $ TypeMismatch "D" "IsForeign"


      "E" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> E <$> read x0
          _ -> Left $ TypeMismatch "E" "IsForeign"


      "F" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> F <$> read x0
          _ -> Left $ TypeMismatch "F" "IsForeign"


      "G" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> G <$> read x0
          _ -> Left $ TypeMismatch "G" "IsForeign"


      "H" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1, x2, x3] -> H <$> read x0 <*> read x1 <*> read x2 <*> read x3
          _ -> Left $ TypeMismatch "H" "IsForeign"


      _ -> Left $ TypeMismatch "SumType" "IsForeign"



instance sumTypeShow :: Show SumType where
  show A = "A"
  show (B x0) = "B: " <> show x0
  show (C x0) = "C: " <> show x0
  show (D x0) = "D: " <> show x0
  show (E x0) = "E: " <> show x0
  show (F x0) = "F: " <> show x0
  show (G x0) = "G: " <> show x0
  show (H x0 x1 x2 x3) = "H: " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3


instance sumTypeEq :: Eq SumType where
  eq A A = true
  eq (B x0a) (B x0b) = x0a == x0b
  eq (C x0a) (C x0b) = x0a == x0b
  eq (D x0a) (D x0b) = x0a == x0b
  eq (E x0a) (E x0b) = x0a == x0b
  eq (F x0a) (F x0b) = x0a == x0b
  eq (G x0a) (G x0b) = x0a == x0b
  eq (H x0a x1a x2a x3a) (H x0b x1b x2b x3b) = x0a == x0b && x1a == x1b && x2a == x2b && x3a == x3b
  eq _ _ = false

newtype BigRecord = BigRecord {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}


type BigRecordR = {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}


_BigRecord :: Lens' BigRecord {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> String -> SumType -> String -> String -> String -> String -> Boolean -> BigRecord
mkBigRecord bigRecordBool bigRecordInt bigRecordMaybeInt bigRecordInteger bigRecordMaybeInteger bigRecordString bigRecordString2 bigRecordSumType bigRecordData bigRecordClass bigRecordLet bigRecordModule bigRecord =
  BigRecord{bigRecordBool, bigRecordInt, bigRecordMaybeInt, bigRecordInteger, bigRecordMaybeInteger, bigRecordString, bigRecordString2, bigRecordSumType, bigRecordData, bigRecordClass, bigRecordLet, bigRecordModule, bigRecord}


unwrapBigRecord :: BigRecord -> {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}
unwrapBigRecord (BigRecord r) = r

instance bigRecordEncodeJson :: EncodeJson BigRecord where
  encodeJson (BigRecord o) =
       "tag" := "BigRecord"
    ~> "bigRecordBool" := o.bigRecordBool
    ~> "bigRecordInt" := o.bigRecordInt
    ~> "bigRecordMaybeInt" := o.bigRecordMaybeInt
    ~> "bigRecordInteger" := o.bigRecordInteger
    ~> "bigRecordMaybeInteger" := o.bigRecordMaybeInteger
    ~> "bigRecordString" := o.bigRecordString
    ~> "bigRecordString2" := o.bigRecordString2
    ~> "bigRecordSumType" := o.bigRecordSumType
    ~> "bigRecordData" := o.bigRecordData
    ~> "bigRecordClass" := o.bigRecordClass
    ~> "bigRecordLet" := o.bigRecordLet
    ~> "bigRecordModule" := o.bigRecordModule
    ~> "bigRecord" := o.bigRecord
    ~> jsonEmptyObject


instance bigRecordDecodeJson :: DecodeJson BigRecord where
  decodeJson o = do
    obj <- decodeJson o
    bigRecordBool <- obj .? "bigRecordBool"
    bigRecordInt <- obj .? "bigRecordInt"
    bigRecordMaybeInt <- obj .? "bigRecordMaybeInt"
    bigRecordInteger <- obj .? "bigRecordInteger"
    bigRecordMaybeInteger <- obj .? "bigRecordMaybeInteger"
    bigRecordString <- obj .? "bigRecordString"
    bigRecordString2 <- obj .? "bigRecordString2"
    bigRecordSumType <- obj .? "bigRecordSumType"
    bigRecordData <- obj .? "bigRecordData"
    bigRecordClass <- obj .? "bigRecordClass"
    bigRecordLet <- obj .? "bigRecordLet"
    bigRecordModule <- obj .? "bigRecordModule"
    bigRecord <- obj .? "bigRecord"
    pure $ BigRecord {
      bigRecordBool,
      bigRecordInt,
      bigRecordMaybeInt,
      bigRecordInteger,
      bigRecordMaybeInteger,
      bigRecordString,
      bigRecordString2,
      bigRecordSumType,
      bigRecordData,
      bigRecordClass,
      bigRecordLet,
      bigRecordModule,
      bigRecord
    }


instance bigRecordRequestable :: Requestable BigRecord where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance bigRecordRespondable :: Respondable BigRecord where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBigRecord
      <$> readProp "bigRecordBool" json
      <*> readProp "bigRecordInt" json
      <*> (unNullOrUndefined <$> readProp "bigRecordMaybeInt" json)
      <*> readProp "bigRecordInteger" json
      <*> (unNullOrUndefined <$> readProp "bigRecordMaybeInteger" json)
      <*> readProp "bigRecordString" json
      <*> readProp "bigRecordString2" json
      <*> readProp "bigRecordSumType" json
      <*> readProp "bigRecordData" json
      <*> readProp "bigRecordClass" json
      <*> readProp "bigRecordLet" json
      <*> readProp "bigRecordModule" json
      <*> readProp "bigRecord" json


instance bigRecordIsForeign :: IsForeign BigRecord where
  read json =
      mkBigRecord
      <$> readProp "bigRecordBool" json
      <*> readProp "bigRecordInt" json
      <*> (unNullOrUndefined <$> readProp "bigRecordMaybeInt" json)
      <*> readProp "bigRecordInteger" json
      <*> (unNullOrUndefined <$> readProp "bigRecordMaybeInteger" json)
      <*> readProp "bigRecordString" json
      <*> readProp "bigRecordString2" json
      <*> readProp "bigRecordSumType" json
      <*> readProp "bigRecordData" json
      <*> readProp "bigRecordClass" json
      <*> readProp "bigRecordLet" json
      <*> readProp "bigRecordModule" json
      <*> readProp "bigRecord" json


instance bigRecordShow :: Show BigRecord where
    show (BigRecord o) = show "bigRecordBool: " <> show o.bigRecordBool <> ", " <> show "bigRecordInt: " <> show o.bigRecordInt <> ", " <> show "bigRecordMaybeInt: " <> show o.bigRecordMaybeInt <> ", " <> show "bigRecordInteger: " <> show o.bigRecordInteger <> ", " <> show "bigRecordMaybeInteger: " <> show o.bigRecordMaybeInteger <> ", " <> show "bigRecordString: " <> show o.bigRecordString <> ", " <> show "bigRecordString2: " <> show o.bigRecordString2 <> ", " <> show "bigRecordSumType: " <> show o.bigRecordSumType <> ", " <> show "bigRecordData: " <> show o.bigRecordData <> ", " <> show "bigRecordClass: " <> show o.bigRecordClass <> ", " <> show "bigRecordLet: " <> show o.bigRecordLet <> ", " <> show "bigRecordModule: " <> show o.bigRecordModule <> ", " <> show "bigRecord: " <> show o.bigRecord

instance bigRecordEq :: Eq BigRecord where
  eq (BigRecord a) (BigRecord b) = a.bigRecordBool == b.bigRecordBool && a.bigRecordInt == b.bigRecordInt && a.bigRecordMaybeInt == b.bigRecordMaybeInt && a.bigRecordInteger == b.bigRecordInteger && a.bigRecordMaybeInteger == b.bigRecordMaybeInteger && a.bigRecordString == b.bigRecordString && a.bigRecordString2 == b.bigRecordString2 && a.bigRecordSumType == b.bigRecordSumType && a.bigRecordData == b.bigRecordData && a.bigRecordClass == b.bigRecordClass && a.bigRecordLet == b.bigRecordLet && a.bigRecordModule == b.bigRecordModule && a.bigRecord == b.bigRecord

type FakeUTCTime  = Int


newtype User = User {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}


type UserR = {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}


_User :: Lens' User {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}
_User f (User o) = User <$> f o


mkUser :: String -> String -> Boolean -> User
mkUser userName userEmail userActive =
  User{userName, userEmail, userActive}


unwrapUser :: User -> {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}
unwrapUser (User r) = r

instance userEncodeJson :: EncodeJson User where
  encodeJson (User o) =
       "tag" := "User"
    ~> "userName" := o.userName
    ~> "userEmail" := o.userEmail
    ~> "userActive" := o.userActive
    ~> jsonEmptyObject


instance userDecodeJson :: DecodeJson User where
  decodeJson o = do
    obj <- decodeJson o
    userName <- obj .? "userName"
    userEmail <- obj .? "userEmail"
    userActive <- obj .? "userActive"
    pure $ User {
      userName,
      userEmail,
      userActive
    }


instance userRequestable :: Requestable User where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userRespondable :: Respondable User where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUser
      <$> readProp "userName" json
      <*> readProp "userEmail" json
      <*> readProp "userActive" json


instance userIsForeign :: IsForeign User where
  read json =
      mkUser
      <$> readProp "userName" json
      <*> readProp "userEmail" json
      <*> readProp "userActive" json


instance userShow :: Show User where
    show (User o) = show "userName: " <> show o.userName <> ", " <> show "userEmail: " <> show o.userEmail <> ", " <> show "userActive: " <> show o.userActive

instance userEq :: Eq User where
  eq (User a) (User b) = a.userName == b.userName && a.userEmail == b.userEmail && a.userActive == b.userActive

newtype UserRequest = UserRequest {
  userRequestName :: String,
  userRequestEmail :: String
}


type UserRequestR = {
  userRequestName :: String,
  userRequestEmail :: String
}


_UserRequest :: Lens' UserRequest {
  userRequestName :: String,
  userRequestEmail :: String
}
_UserRequest f (UserRequest o) = UserRequest <$> f o


mkUserRequest :: String -> String -> UserRequest
mkUserRequest userRequestName userRequestEmail =
  UserRequest{userRequestName, userRequestEmail}


unwrapUserRequest :: UserRequest -> {
  userRequestName :: String,
  userRequestEmail :: String
}
unwrapUserRequest (UserRequest r) = r

instance userRequestEncodeJson :: EncodeJson UserRequest where
  encodeJson (UserRequest o) =
       "tag" := "UserRequest"
    ~> "userRequestName" := o.userRequestName
    ~> "userRequestEmail" := o.userRequestEmail
    ~> jsonEmptyObject


instance userRequestDecodeJson :: DecodeJson UserRequest where
  decodeJson o = do
    obj <- decodeJson o
    userRequestName <- obj .? "userRequestName"
    userRequestEmail <- obj .? "userRequestEmail"
    pure $ UserRequest {
      userRequestName,
      userRequestEmail
    }


instance userRequestRequestable :: Requestable UserRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userRequestRespondable :: Respondable UserRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserRequest
      <$> readProp "userRequestName" json
      <*> readProp "userRequestEmail" json


instance userRequestIsForeign :: IsForeign UserRequest where
  read json =
      mkUserRequest
      <$> readProp "userRequestName" json
      <*> readProp "userRequestEmail" json


instance userRequestShow :: Show UserRequest where
    show (UserRequest o) = show "userRequestName: " <> show o.userRequestName <> ", " <> show "userRequestEmail: " <> show o.userRequestEmail

instance userRequestEq :: Eq UserRequest where
  eq (UserRequest a) (UserRequest b) = a.userRequestName == b.userRequestName && a.userRequestEmail == b.userRequestEmail

newtype UserResponse = UserResponse {
  userResponseId :: Int,
  userResponseName :: String,
  userResponseEmail :: String,
  userResponseActive :: Boolean,
  userResponseCreatedAt :: (Maybe FakeUTCTime),
  userResponseModifiedAt :: (Maybe FakeUTCTime)
}


type UserResponseR = {
  userResponseId :: Int,
  userResponseName :: String,
  userResponseEmail :: String,
  userResponseActive :: Boolean,
  userResponseCreatedAt :: (Maybe FakeUTCTime),
  userResponseModifiedAt :: (Maybe FakeUTCTime)
}


_UserResponse :: Lens' UserResponse {
  userResponseId :: Int,
  userResponseName :: String,
  userResponseEmail :: String,
  userResponseActive :: Boolean,
  userResponseCreatedAt :: (Maybe FakeUTCTime),
  userResponseModifiedAt :: (Maybe FakeUTCTime)
}
_UserResponse f (UserResponse o) = UserResponse <$> f o


mkUserResponse :: Int -> String -> String -> Boolean -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserResponse
mkUserResponse userResponseId userResponseName userResponseEmail userResponseActive userResponseCreatedAt userResponseModifiedAt =
  UserResponse{userResponseId, userResponseName, userResponseEmail, userResponseActive, userResponseCreatedAt, userResponseModifiedAt}


unwrapUserResponse :: UserResponse -> {
  userResponseId :: Int,
  userResponseName :: String,
  userResponseEmail :: String,
  userResponseActive :: Boolean,
  userResponseCreatedAt :: (Maybe FakeUTCTime),
  userResponseModifiedAt :: (Maybe FakeUTCTime)
}
unwrapUserResponse (UserResponse r) = r

instance userResponseEncodeJson :: EncodeJson UserResponse where
  encodeJson (UserResponse o) =
       "tag" := "UserResponse"
    ~> "userResponseId" := o.userResponseId
    ~> "userResponseName" := o.userResponseName
    ~> "userResponseEmail" := o.userResponseEmail
    ~> "userResponseActive" := o.userResponseActive
    ~> "userResponseCreatedAt" := o.userResponseCreatedAt
    ~> "userResponseModifiedAt" := o.userResponseModifiedAt
    ~> jsonEmptyObject


instance userResponseDecodeJson :: DecodeJson UserResponse where
  decodeJson o = do
    obj <- decodeJson o
    userResponseId <- obj .? "userResponseId"
    userResponseName <- obj .? "userResponseName"
    userResponseEmail <- obj .? "userResponseEmail"
    userResponseActive <- obj .? "userResponseActive"
    userResponseCreatedAt <- obj .? "userResponseCreatedAt"
    userResponseModifiedAt <- obj .? "userResponseModifiedAt"
    pure $ UserResponse {
      userResponseId,
      userResponseName,
      userResponseEmail,
      userResponseActive,
      userResponseCreatedAt,
      userResponseModifiedAt
    }


instance userResponseRequestable :: Requestable UserResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userResponseRespondable :: Respondable UserResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserResponse
      <$> readProp "userResponseId" json
      <*> readProp "userResponseName" json
      <*> readProp "userResponseEmail" json
      <*> readProp "userResponseActive" json
      <*> (unNullOrUndefined <$> readProp "userResponseCreatedAt" json)
      <*> (unNullOrUndefined <$> readProp "userResponseModifiedAt" json)


instance userResponseIsForeign :: IsForeign UserResponse where
  read json =
      mkUserResponse
      <$> readProp "userResponseId" json
      <*> readProp "userResponseName" json
      <*> readProp "userResponseEmail" json
      <*> readProp "userResponseActive" json
      <*> (unNullOrUndefined <$> readProp "userResponseCreatedAt" json)
      <*> (unNullOrUndefined <$> readProp "userResponseModifiedAt" json)


instance userResponseShow :: Show UserResponse where
    show (UserResponse o) = show "userResponseId: " <> show o.userResponseId <> ", " <> show "userResponseName: " <> show o.userResponseName <> ", " <> show "userResponseEmail: " <> show o.userResponseEmail <> ", " <> show "userResponseActive: " <> show o.userResponseActive <> ", " <> show "userResponseCreatedAt: " <> show o.userResponseCreatedAt <> ", " <> show "userResponseModifiedAt: " <> show o.userResponseModifiedAt

instance userResponseEq :: Eq UserResponse where
  eq (UserResponse a) (UserResponse b) = a.userResponseId == b.userResponseId && a.userResponseName == b.userResponseName && a.userResponseEmail == b.userResponseEmail && a.userResponseActive == b.userResponseActive && a.userResponseCreatedAt == b.userResponseCreatedAt && a.userResponseModifiedAt == b.userResponseModifiedAt

newtype UserResponses = UserResponses {
  userResponses :: (Array UserResponse)
}


type UserResponsesR = {
  userResponses :: (Array UserResponse)
}


_UserResponses :: Lens' UserResponses {
  userResponses :: (Array UserResponse)
}
_UserResponses f (UserResponses o) = UserResponses <$> f o


mkUserResponses :: (Array UserResponse) -> UserResponses
mkUserResponses userResponses =
  UserResponses{userResponses}


unwrapUserResponses :: UserResponses -> {
  userResponses :: (Array UserResponse)
}
unwrapUserResponses (UserResponses r) = r

instance userResponsesEncodeJson :: EncodeJson UserResponses where
  encodeJson (UserResponses o) =
       "tag" := "UserResponses"
    ~> "userResponses" := o.userResponses
    ~> jsonEmptyObject


instance userResponsesDecodeJson :: DecodeJson UserResponses where
  decodeJson o = do
    obj <- decodeJson o
    userResponses <- obj .? "userResponses"
    pure $ UserResponses {
      userResponses
    }


instance userResponsesRequestable :: Requestable UserResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userResponsesRespondable :: Respondable UserResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserResponses
      <$> readProp "userResponses" json


instance userResponsesIsForeign :: IsForeign UserResponses where
  read json =
      mkUserResponses
      <$> readProp "userResponses" json


instance userResponsesShow :: Show UserResponses where
    show (UserResponses o) = show "userResponses: " <> show o.userResponses

instance userResponsesEq :: Eq UserResponses where
  eq (UserResponses a) (UserResponses b) = a.userResponses == b.userResponses

type Text  = String


type TextMaybe  = (Maybe String)


type NestedList a = (Array (Array a))


newtype FunkyRecord = Boom1 {
  boom1 :: Boolean
}


type FunkyRecordR = {
  boom1 :: Boolean
}


_FunkyRecord :: Lens' FunkyRecord {
  boom1 :: Boolean
}
_FunkyRecord f (Boom1 o) = Boom1 <$> f o


mkFunkyRecord :: Boolean -> FunkyRecord
mkFunkyRecord boom1 =
  Boom1{boom1}


unwrapFunkyRecord :: FunkyRecord -> {
  boom1 :: Boolean
}
unwrapFunkyRecord (Boom1 r) = r

instance funkyRecordEncodeJson :: EncodeJson FunkyRecord where
  encodeJson (Boom1 o) =
       "tag" := "FunkyRecord"
    ~> "boom1" := o.boom1
    ~> jsonEmptyObject


instance funkyRecordDecodeJson :: DecodeJson FunkyRecord where
  decodeJson o = do
    obj <- decodeJson o
    boom1 <- obj .? "boom1"
    pure $ Boom1 {
      boom1
    }


instance funkyRecordRequestable :: Requestable FunkyRecord where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance funkyRecordRespondable :: Respondable FunkyRecord where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkFunkyRecord
      <$> readProp "boom1" json


instance funkyRecordIsForeign :: IsForeign FunkyRecord where
  read json =
      mkFunkyRecord
      <$> readProp "boom1" json


instance funkyRecordShow :: Show FunkyRecord where
    show (Boom1 o) = show "boom1: " <> show o.boom1

instance funkyRecordEq :: Eq FunkyRecord where
  eq (Boom1 a) (Boom1 b) = a.boom1 == b.boom1

newtype FUnkyRecordP = FUnkyRecordP {
  funkyrecordpField :: Boolean
}


type FUnkyRecordPR = {
  funkyrecordpField :: Boolean
}


_FUnkyRecordP :: Lens' FUnkyRecordP {
  funkyrecordpField :: Boolean
}
_FUnkyRecordP f (FUnkyRecordP o) = FUnkyRecordP <$> f o


mkFUnkyRecordP :: Boolean -> FUnkyRecordP
mkFUnkyRecordP funkyrecordpField =
  FUnkyRecordP{funkyrecordpField}


unwrapFUnkyRecordP :: FUnkyRecordP -> {
  funkyrecordpField :: Boolean
}
unwrapFUnkyRecordP (FUnkyRecordP r) = r

instance fUnkyRecordPEncodeJson :: EncodeJson FUnkyRecordP where
  encodeJson (FUnkyRecordP o) =
       "tag" := "FUnkyRecordP"
    ~> "funkyrecordpField" := o.funkyrecordpField
    ~> jsonEmptyObject


instance fUnkyRecordPDecodeJson :: DecodeJson FUnkyRecordP where
  decodeJson o = do
    obj <- decodeJson o
    funkyrecordpField <- obj .? "funkyrecordpField"
    pure $ FUnkyRecordP {
      funkyrecordpField
    }


instance fUnkyRecordPRequestable :: Requestable FUnkyRecordP where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance fUnkyRecordPRespondable :: Respondable FUnkyRecordP where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkFUnkyRecordP
      <$> readProp "funkyrecordpField" json


instance fUnkyRecordPIsForeign :: IsForeign FUnkyRecordP where
  read json =
      mkFUnkyRecordP
      <$> readProp "funkyrecordpField" json


instance fUnkyRecordPShow :: Show FUnkyRecordP where
    show (FUnkyRecordP o) = show "funkyrecordpField: " <> show o.funkyrecordpField

instance fUnkyRecordPEq :: Eq FUnkyRecordP where
  eq (FUnkyRecordP a) (FUnkyRecordP b) = a.funkyrecordpField == b.funkyrecordpField

data Param
  = Limit Int
  | Offset Int
  | ByUsersIds (Array Int)
  | ByUsersNames (Array String)
  | ByUsersEmails (Array String)
  | ByUserActive Boolean



instance paramEncodeJson :: EncodeJson Param where
  encodeJson (Limit x0) =
       "tag" := "Limit"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Offset x0) =
       "tag" := "Offset"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUsersIds x0) =
       "tag" := "ByUsersIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUsersNames x0) =
       "tag" := "ByUsersNames"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUsersEmails x0) =
       "tag" := "ByUsersEmails"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUserActive x0) =
       "tag" := "ByUserActive"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject


instance paramDecodeJson :: DecodeJson Param where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Limit" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Limit <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Limit"


      "Offset" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Offset <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Offset"


      "ByUsersIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUsersIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUsersIds"


      "ByUsersNames" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUsersNames <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUsersNames"


      "ByUsersEmails" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUsersEmails <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUsersEmails"


      "ByUserActive" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUserActive <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUserActive"


      _ -> Left $ "DecodeJson TypeMismatch for Param"



instance paramRequestable :: Requestable Param where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance paramRespondable :: Respondable Param where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Limit" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> Limit <$> read x0
          _ -> Left $ TypeMismatch "Limit" "Respondable"


      "Offset" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> Offset <$> read x0
          _ -> Left $ TypeMismatch "Offset" "Respondable"


      "ByUsersIds" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersIds <$> read x0
          _ -> Left $ TypeMismatch "ByUsersIds" "Respondable"


      "ByUsersNames" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersNames <$> read x0
          _ -> Left $ TypeMismatch "ByUsersNames" "Respondable"


      "ByUsersEmails" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersEmails <$> read x0
          _ -> Left $ TypeMismatch "ByUsersEmails" "Respondable"


      "ByUserActive" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUserActive <$> read x0
          _ -> Left $ TypeMismatch "ByUserActive" "Respondable"


      _ -> Left $ TypeMismatch "Param" "Respondable"



instance paramIsForeign :: IsForeign Param where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Limit" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> Limit <$> read x0
          _ -> Left $ TypeMismatch "Limit" "IsForeign"


      "Offset" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> Offset <$> read x0
          _ -> Left $ TypeMismatch "Offset" "IsForeign"


      "ByUsersIds" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersIds <$> read x0
          _ -> Left $ TypeMismatch "ByUsersIds" "IsForeign"


      "ByUsersNames" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersNames <$> read x0
          _ -> Left $ TypeMismatch "ByUsersNames" "IsForeign"


      "ByUsersEmails" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUsersEmails <$> read x0
          _ -> Left $ TypeMismatch "ByUsersEmails" "IsForeign"


      "ByUserActive" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ByUserActive <$> read x0
          _ -> Left $ TypeMismatch "ByUserActive" "IsForeign"


      _ -> Left $ TypeMismatch "Param" "IsForeign"



instance paramShow :: Show Param where
  show (Limit x0) = "Limit: " <> show x0
  show (Offset x0) = "Offset: " <> show x0
  show (ByUsersIds x0) = "ByUsersIds: " <> show x0
  show (ByUsersNames x0) = "ByUsersNames: " <> show x0
  show (ByUsersEmails x0) = "ByUsersEmails: " <> show x0
  show (ByUserActive x0) = "ByUserActive: " <> show x0


instance paramEq :: Eq Param where
  eq (Limit x0a) (Limit x0b) = x0a == x0b
  eq (Offset x0a) (Offset x0b) = x0a == x0b
  eq (ByUsersIds x0a) (ByUsersIds x0b) = x0a == x0b
  eq (ByUsersNames x0a) (ByUsersNames x0b) = x0a == x0b
  eq (ByUsersEmails x0a) (ByUsersEmails x0b) = x0a == x0b
  eq (ByUserActive x0a) (ByUserActive x0b) = x0a == x0b
  eq _ _ = false

data ParamTag
  = ParamTag_Limit 
  | ParamTag_Offset 
  | ParamTag_ByUsersIds 
  | ParamTag_ByUsersNames 
  | ParamTag_ByUsersEmails 
  | ParamTag_ByUserActive 



instance paramTagEncodeJson :: EncodeJson ParamTag where
  encodeJson (ParamTag_Limit ) =
       "tag" := "ParamTag_Limit"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_Offset ) =
       "tag" := "ParamTag_Offset"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUsersIds ) =
       "tag" := "ParamTag_ByUsersIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUsersNames ) =
       "tag" := "ParamTag_ByUsersNames"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUsersEmails ) =
       "tag" := "ParamTag_ByUsersEmails"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUserActive ) =
       "tag" := "ParamTag_ByUserActive"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance paramTagDecodeJson :: DecodeJson ParamTag where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByUsersEmails" -> do
        pure ParamTag_ByUsersEmails

      "ParamTag_ByUserActive" -> do
        pure ParamTag_ByUserActive

      _ -> Left $ "DecodeJson TypeMismatch for ParamTag"



instance paramTagRequestable :: Requestable ParamTag where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance paramTagRespondable :: Respondable ParamTag where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByUsersEmails" -> do
        pure ParamTag_ByUsersEmails

      "ParamTag_ByUserActive" -> do
        pure ParamTag_ByUserActive

      _ -> Left $ TypeMismatch "ParamTag" "Respondable"



instance paramTagIsForeign :: IsForeign ParamTag where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByUsersEmails" -> do
        pure ParamTag_ByUsersEmails

      "ParamTag_ByUserActive" -> do
        pure ParamTag_ByUserActive

      _ -> Left $ TypeMismatch "ParamTag" "IsForeign"



instance paramTagShow :: Show ParamTag where
  show ParamTag_Limit = "ParamTag_Limit"
  show ParamTag_Offset = "ParamTag_Offset"
  show ParamTag_ByUsersIds = "ParamTag_ByUsersIds"
  show ParamTag_ByUsersNames = "ParamTag_ByUsersNames"
  show ParamTag_ByUsersEmails = "ParamTag_ByUsersEmails"
  show ParamTag_ByUserActive = "ParamTag_ByUserActive"


readParamTag :: String -> Maybe ParamTag
readParamTag "ParamTag_Limit" = Just ParamTag_Limit
readParamTag "ParamTag_Offset" = Just ParamTag_Offset
readParamTag "ParamTag_ByUsersIds" = Just ParamTag_ByUsersIds
readParamTag "ParamTag_ByUsersNames" = Just ParamTag_ByUsersNames
readParamTag "ParamTag_ByUsersEmails" = Just ParamTag_ByUsersEmails
readParamTag "ParamTag_ByUserActive" = Just ParamTag_ByUserActive
readParamTag _ = Nothing

instance paramTagEq :: Eq ParamTag where
  eq ParamTag_Limit ParamTag_Limit = true
  eq ParamTag_Offset ParamTag_Offset = true
  eq ParamTag_ByUsersIds ParamTag_ByUsersIds = true
  eq ParamTag_ByUsersNames ParamTag_ByUsersNames = true
  eq ParamTag_ByUsersEmails ParamTag_ByUsersEmails = true
  eq ParamTag_ByUserActive ParamTag_ByUserActive = true
  eq _ _ = false

bigRecord_ :: forall b a r. Lens { bigRecord :: a | r } { bigRecord :: b | r } a b
bigRecord_ f o = o { bigRecord = _ } <$> f o.bigRecord


bigRecordBool_ :: forall b a r. Lens { bigRecordBool :: a | r } { bigRecordBool :: b | r } a b
bigRecordBool_ f o = o { bigRecordBool = _ } <$> f o.bigRecordBool


bigRecordClass_ :: forall b a r. Lens { bigRecordClass :: a | r } { bigRecordClass :: b | r } a b
bigRecordClass_ f o = o { bigRecordClass = _ } <$> f o.bigRecordClass


bigRecordData_ :: forall b a r. Lens { bigRecordData :: a | r } { bigRecordData :: b | r } a b
bigRecordData_ f o = o { bigRecordData = _ } <$> f o.bigRecordData


bigRecordInt_ :: forall b a r. Lens { bigRecordInt :: a | r } { bigRecordInt :: b | r } a b
bigRecordInt_ f o = o { bigRecordInt = _ } <$> f o.bigRecordInt


bigRecordInteger_ :: forall b a r. Lens { bigRecordInteger :: a | r } { bigRecordInteger :: b | r } a b
bigRecordInteger_ f o = o { bigRecordInteger = _ } <$> f o.bigRecordInteger


bigRecordLet_ :: forall b a r. Lens { bigRecordLet :: a | r } { bigRecordLet :: b | r } a b
bigRecordLet_ f o = o { bigRecordLet = _ } <$> f o.bigRecordLet


bigRecordMaybeInt_ :: forall b a r. Lens { bigRecordMaybeInt :: a | r } { bigRecordMaybeInt :: b | r } a b
bigRecordMaybeInt_ f o = o { bigRecordMaybeInt = _ } <$> f o.bigRecordMaybeInt


bigRecordMaybeInteger_ :: forall b a r. Lens { bigRecordMaybeInteger :: a | r } { bigRecordMaybeInteger :: b | r } a b
bigRecordMaybeInteger_ f o = o { bigRecordMaybeInteger = _ } <$> f o.bigRecordMaybeInteger


bigRecordModule_ :: forall b a r. Lens { bigRecordModule :: a | r } { bigRecordModule :: b | r } a b
bigRecordModule_ f o = o { bigRecordModule = _ } <$> f o.bigRecordModule


bigRecordString_ :: forall b a r. Lens { bigRecordString :: a | r } { bigRecordString :: b | r } a b
bigRecordString_ f o = o { bigRecordString = _ } <$> f o.bigRecordString


bigRecordString2_ :: forall b a r. Lens { bigRecordString2 :: a | r } { bigRecordString2 :: b | r } a b
bigRecordString2_ f o = o { bigRecordString2 = _ } <$> f o.bigRecordString2


bigRecordSumType_ :: forall b a r. Lens { bigRecordSumType :: a | r } { bigRecordSumType :: b | r } a b
bigRecordSumType_ f o = o { bigRecordSumType = _ } <$> f o.bigRecordSumType


boom1_ :: forall b a r. Lens { boom1 :: a | r } { boom1 :: b | r } a b
boom1_ f o = o { boom1 = _ } <$> f o.boom1


funkyrecordpField_ :: forall b a r. Lens { funkyrecordpField :: a | r } { funkyrecordpField :: b | r } a b
funkyrecordpField_ f o = o { funkyrecordpField = _ } <$> f o.funkyrecordpField


unSession_ :: forall b a r. Lens { unSession :: a | r } { unSession :: b | r } a b
unSession_ f o = o { unSession = _ } <$> f o.unSession


userActive_ :: forall b a r. Lens { userActive :: a | r } { userActive :: b | r } a b
userActive_ f o = o { userActive = _ } <$> f o.userActive


userEmail_ :: forall b a r. Lens { userEmail :: a | r } { userEmail :: b | r } a b
userEmail_ f o = o { userEmail = _ } <$> f o.userEmail


userName_ :: forall b a r. Lens { userName :: a | r } { userName :: b | r } a b
userName_ f o = o { userName = _ } <$> f o.userName


userRequestEmail_ :: forall b a r. Lens { userRequestEmail :: a | r } { userRequestEmail :: b | r } a b
userRequestEmail_ f o = o { userRequestEmail = _ } <$> f o.userRequestEmail


userRequestName_ :: forall b a r. Lens { userRequestName :: a | r } { userRequestName :: b | r } a b
userRequestName_ f o = o { userRequestName = _ } <$> f o.userRequestName


userResponseActive_ :: forall b a r. Lens { userResponseActive :: a | r } { userResponseActive :: b | r } a b
userResponseActive_ f o = o { userResponseActive = _ } <$> f o.userResponseActive


userResponseCreatedAt_ :: forall b a r. Lens { userResponseCreatedAt :: a | r } { userResponseCreatedAt :: b | r } a b
userResponseCreatedAt_ f o = o { userResponseCreatedAt = _ } <$> f o.userResponseCreatedAt


userResponseEmail_ :: forall b a r. Lens { userResponseEmail :: a | r } { userResponseEmail :: b | r } a b
userResponseEmail_ f o = o { userResponseEmail = _ } <$> f o.userResponseEmail


userResponseId_ :: forall b a r. Lens { userResponseId :: a | r } { userResponseId :: b | r } a b
userResponseId_ f o = o { userResponseId = _ } <$> f o.userResponseId


userResponseModifiedAt_ :: forall b a r. Lens { userResponseModifiedAt :: a | r } { userResponseModifiedAt :: b | r } a b
userResponseModifiedAt_ f o = o { userResponseModifiedAt = _ } <$> f o.userResponseModifiedAt


userResponseName_ :: forall b a r. Lens { userResponseName :: a | r } { userResponseName :: b | r } a b
userResponseName_ f o = o { userResponseName = _ } <$> f o.userResponseName


userResponses_ :: forall b a r. Lens { userResponses :: a | r } { userResponses :: b | r } a b
userResponses_ f o = o { userResponses = _ } <$> f o.userResponses

-- footer
module Interop.Clean where


import Control.Monad.Aff                ()
import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date(..))
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..))
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class IsForeign, read, readProp)
import Data.List                        (List ())
import Data.Maybe                       (Maybe(..))
import Data.Set                         (Set ())
import Data.Tuple                       (Tuple(..))
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==))

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
    ~> "un_session" := o.unSession
    ~> jsonEmptyObject


instance sessionDecodeJson :: DecodeJson Session where
  decodeJson o = do
    obj <- decodeJson o
    unSession <- obj .? "un_session"
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
      <$> readProp "un_session" json


instance sessionIsForeign :: IsForeign Session where
  read json =
      mkSession
      <$> readProp "un_session" json


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
  show (A) = "A"
  show (B x0) = "B: " <> show x0
  show (C x0) = "C: " <> show x0
  show (D x0) = "D: " <> show x0
  show (E x0) = "E: " <> show x0
  show (F x0) = "F: " <> show x0
  show (G x0) = "G: " <> show x0
  show (H x0 x1 x2 x3) = "H: " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3


instance sumTypeEq :: Eq SumType where
  eq (A) (A) = true
  eq (B x0a) (B x0b) = x0a == x0b
  eq (C x0a) (C x0b) = x0a == x0b
  eq (D x0a) (D x0b) = x0a == x0b
  eq (E x0a) (E x0b) = x0a == x0b
  eq (F x0a) (F x0b) = x0a == x0b
  eq (G x0a) (G x0b) = x0a == x0b
  eq (H x0a x1a x2a x3a) (H x0b x1b x2b x3b) = x0a == x0b && x1a == x1b && x2a == x2b && x3a == x3b
  eq _ _ = false

newtype BigRecord = BigRecord {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  string2 :: String,
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}


type BigRecordR = {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  string2 :: String,
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}


_BigRecord :: Lens' BigRecord {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  string2 :: String,
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> String -> SumType -> String -> String -> String -> String -> Boolean -> BigRecord
mkBigRecord bool int maybeInt integer maybeInteger string string2 sumType dataP classP letP moduleP bigRecord =
  BigRecord{bool, int, maybeInt, integer, maybeInteger, string, string2, sumType, dataP, classP, letP, moduleP, bigRecord}


unwrapBigRecord :: BigRecord -> {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  string2 :: String,
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}
unwrapBigRecord (BigRecord r) = r

instance bigRecordEncodeJson :: EncodeJson BigRecord where
  encodeJson (BigRecord o) =
       "tag" := "BigRecord"
    ~> "bool" := o.bool
    ~> "int" := o.int
    ~> "maybe_int" := o.maybeInt
    ~> "integer" := o.integer
    ~> "maybe_integer" := o.maybeInteger
    ~> "string" := o.string
    ~> "string2" := o.string2
    ~> "sum_type" := o.sumType
    ~> "data" := o.dataP
    ~> "class" := o.classP
    ~> "let" := o.letP
    ~> "module" := o.moduleP
    ~> "big_record" := o.bigRecord
    ~> jsonEmptyObject


instance bigRecordDecodeJson :: DecodeJson BigRecord where
  decodeJson o = do
    obj <- decodeJson o
    bool <- obj .? "bool"
    int <- obj .? "int"
    maybeInt <- obj .? "maybe_int"
    integer <- obj .? "integer"
    maybeInteger <- obj .? "maybe_integer"
    string <- obj .? "string"
    string2 <- obj .? "string2"
    sumType <- obj .? "sum_type"
    dataP <- obj .? "data"
    classP <- obj .? "class"
    letP <- obj .? "let"
    moduleP <- obj .? "module"
    bigRecord <- obj .? "big_record"
    pure $ BigRecord {
      bool,
      int,
      maybeInt,
      integer,
      maybeInteger,
      string,
      string2,
      sumType,
      dataP,
      classP,
      letP,
      moduleP,
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
      <$> readProp "bool" json
      <*> readProp "int" json
      <*> (unNullOrUndefined <$> readProp "maybe_int" json)
      <*> readProp "integer" json
      <*> (unNullOrUndefined <$> readProp "maybe_integer" json)
      <*> readProp "string" json
      <*> readProp "string2" json
      <*> readProp "sum_type" json
      <*> readProp "data" json
      <*> readProp "class" json
      <*> readProp "let" json
      <*> readProp "module" json
      <*> readProp "big_record" json


instance bigRecordIsForeign :: IsForeign BigRecord where
  read json =
      mkBigRecord
      <$> readProp "bool" json
      <*> readProp "int" json
      <*> (unNullOrUndefined <$> readProp "maybe_int" json)
      <*> readProp "integer" json
      <*> (unNullOrUndefined <$> readProp "maybe_integer" json)
      <*> readProp "string" json
      <*> readProp "string2" json
      <*> readProp "sum_type" json
      <*> readProp "data" json
      <*> readProp "class" json
      <*> readProp "let" json
      <*> readProp "module" json
      <*> readProp "big_record" json


instance bigRecordShow :: Show BigRecord where
    show (BigRecord o) = show "bool: " <> show o.bool <> ", " <> show "int: " <> show o.int <> ", " <> show "maybeInt: " <> show o.maybeInt <> ", " <> show "integer: " <> show o.integer <> ", " <> show "maybeInteger: " <> show o.maybeInteger <> ", " <> show "string: " <> show o.string <> ", " <> show "string2: " <> show o.string2 <> ", " <> show "sumType: " <> show o.sumType <> ", " <> show "dataP: " <> show o.dataP <> ", " <> show "classP: " <> show o.classP <> ", " <> show "letP: " <> show o.letP <> ", " <> show "moduleP: " <> show o.moduleP <> ", " <> show "bigRecord: " <> show o.bigRecord

instance bigRecordEq :: Eq BigRecord where
  eq (BigRecord a) (BigRecord b) = a.bool == b.bool && a.int == b.int && a.maybeInt == b.maybeInt && a.integer == b.integer && a.maybeInteger == b.maybeInteger && a.string == b.string && a.string2 == b.string2 && a.sumType == b.sumType && a.dataP == b.dataP && a.classP == b.classP && a.letP == b.letP && a.moduleP == b.moduleP && a.bigRecord == b.bigRecord

type FakeUTCTime  = Int


newtype User = User {
  name :: String,
  email :: String,
  active :: Boolean
}


type UserR = {
  name :: String,
  email :: String,
  active :: Boolean
}


_User :: Lens' User {
  name :: String,
  email :: String,
  active :: Boolean
}
_User f (User o) = User <$> f o


mkUser :: String -> String -> Boolean -> User
mkUser name email active =
  User{name, email, active}


unwrapUser :: User -> {
  name :: String,
  email :: String,
  active :: Boolean
}
unwrapUser (User r) = r

instance userEncodeJson :: EncodeJson User where
  encodeJson (User o) =
       "tag" := "User"
    ~> "name" := o.name
    ~> "email" := o.email
    ~> "active" := o.active
    ~> jsonEmptyObject


instance userDecodeJson :: DecodeJson User where
  decodeJson o = do
    obj <- decodeJson o
    name <- obj .? "name"
    email <- obj .? "email"
    active <- obj .? "active"
    pure $ User {
      name,
      email,
      active
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
      <$> readProp "name" json
      <*> readProp "email" json
      <*> readProp "active" json


instance userIsForeign :: IsForeign User where
  read json =
      mkUser
      <$> readProp "name" json
      <*> readProp "email" json
      <*> readProp "active" json


instance userShow :: Show User where
    show (User o) = show "name: " <> show o.name <> ", " <> show "email: " <> show o.email <> ", " <> show "active: " <> show o.active

instance userEq :: Eq User where
  eq (User a) (User b) = a.name == b.name && a.email == b.email && a.active == b.active

newtype UserRequest = UserRequest {
  name :: String,
  email :: String
}


type UserRequestR = {
  name :: String,
  email :: String
}


_UserRequest :: Lens' UserRequest {
  name :: String,
  email :: String
}
_UserRequest f (UserRequest o) = UserRequest <$> f o


mkUserRequest :: String -> String -> UserRequest
mkUserRequest name email =
  UserRequest{name, email}


unwrapUserRequest :: UserRequest -> {
  name :: String,
  email :: String
}
unwrapUserRequest (UserRequest r) = r

instance userRequestEncodeJson :: EncodeJson UserRequest where
  encodeJson (UserRequest o) =
       "tag" := "UserRequest"
    ~> "name" := o.name
    ~> "email" := o.email
    ~> jsonEmptyObject


instance userRequestDecodeJson :: DecodeJson UserRequest where
  decodeJson o = do
    obj <- decodeJson o
    name <- obj .? "name"
    email <- obj .? "email"
    pure $ UserRequest {
      name,
      email
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
      <$> readProp "name" json
      <*> readProp "email" json


instance userRequestIsForeign :: IsForeign UserRequest where
  read json =
      mkUserRequest
      <$> readProp "name" json
      <*> readProp "email" json


instance userRequestShow :: Show UserRequest where
    show (UserRequest o) = show "name: " <> show o.name <> ", " <> show "email: " <> show o.email

instance userRequestEq :: Eq UserRequest where
  eq (UserRequest a) (UserRequest b) = a.name == b.name && a.email == b.email

newtype UserResponse = UserResponse {
  id :: Int,
  name :: String,
  email :: String,
  active :: Boolean,
  createdAt :: (Maybe FakeUTCTime),
  modifiedAt :: (Maybe FakeUTCTime)
}


type UserResponseR = {
  id :: Int,
  name :: String,
  email :: String,
  active :: Boolean,
  createdAt :: (Maybe FakeUTCTime),
  modifiedAt :: (Maybe FakeUTCTime)
}


_UserResponse :: Lens' UserResponse {
  id :: Int,
  name :: String,
  email :: String,
  active :: Boolean,
  createdAt :: (Maybe FakeUTCTime),
  modifiedAt :: (Maybe FakeUTCTime)
}
_UserResponse f (UserResponse o) = UserResponse <$> f o


mkUserResponse :: Int -> String -> String -> Boolean -> (Maybe FakeUTCTime) -> (Maybe FakeUTCTime) -> UserResponse
mkUserResponse id name email active createdAt modifiedAt =
  UserResponse{id, name, email, active, createdAt, modifiedAt}


unwrapUserResponse :: UserResponse -> {
  id :: Int,
  name :: String,
  email :: String,
  active :: Boolean,
  createdAt :: (Maybe FakeUTCTime),
  modifiedAt :: (Maybe FakeUTCTime)
}
unwrapUserResponse (UserResponse r) = r

instance userResponseEncodeJson :: EncodeJson UserResponse where
  encodeJson (UserResponse o) =
       "tag" := "UserResponse"
    ~> "id" := o.id
    ~> "name" := o.name
    ~> "email" := o.email
    ~> "active" := o.active
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance userResponseDecodeJson :: DecodeJson UserResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    name <- obj .? "name"
    email <- obj .? "email"
    active <- obj .? "active"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ UserResponse {
      id,
      name,
      email,
      active,
      createdAt,
      modifiedAt
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
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "email" json
      <*> readProp "active" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance userResponseIsForeign :: IsForeign UserResponse where
  read json =
      mkUserResponse
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "email" json
      <*> readProp "active" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance userResponseShow :: Show UserResponse where
    show (UserResponse o) = show "id: " <> show o.id <> ", " <> show "name: " <> show o.name <> ", " <> show "email: " <> show o.email <> ", " <> show "active: " <> show o.active <> ", " <> show "createdAt: " <> show o.createdAt <> ", " <> show "modifiedAt: " <> show o.modifiedAt

instance userResponseEq :: Eq UserResponse where
  eq (UserResponse a) (UserResponse b) = a.id == b.id && a.name == b.name && a.email == b.email && a.active == b.active && a.createdAt == b.createdAt && a.modifiedAt == b.modifiedAt

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
    ~> "user_responses" := o.userResponses
    ~> jsonEmptyObject


instance userResponsesDecodeJson :: DecodeJson UserResponses where
  decodeJson o = do
    obj <- decodeJson o
    userResponses <- obj .? "user_responses"
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
      <$> readProp "user_responses" json


instance userResponsesIsForeign :: IsForeign UserResponses where
  read json =
      mkUserResponses
      <$> readProp "user_responses" json


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
  field :: Boolean
}


type FUnkyRecordPR = {
  field :: Boolean
}


_FUnkyRecordP :: Lens' FUnkyRecordP {
  field :: Boolean
}
_FUnkyRecordP f (FUnkyRecordP o) = FUnkyRecordP <$> f o


mkFUnkyRecordP :: Boolean -> FUnkyRecordP
mkFUnkyRecordP field =
  FUnkyRecordP{field}


unwrapFUnkyRecordP :: FUnkyRecordP -> {
  field :: Boolean
}
unwrapFUnkyRecordP (FUnkyRecordP r) = r

instance fUnkyRecordPEncodeJson :: EncodeJson FUnkyRecordP where
  encodeJson (FUnkyRecordP o) =
       "tag" := "FUnkyRecordP"
    ~> "field" := o.field
    ~> jsonEmptyObject


instance fUnkyRecordPDecodeJson :: DecodeJson FUnkyRecordP where
  decodeJson o = do
    obj <- decodeJson o
    field <- obj .? "field"
    pure $ FUnkyRecordP {
      field
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
      <$> readProp "field" json


instance fUnkyRecordPIsForeign :: IsForeign FUnkyRecordP where
  read json =
      mkFUnkyRecordP
      <$> readProp "field" json


instance fUnkyRecordPShow :: Show FUnkyRecordP where
    show (FUnkyRecordP o) = show "field: " <> show o.field

instance fUnkyRecordPEq :: Eq FUnkyRecordP where
  eq (FUnkyRecordP a) (FUnkyRecordP b) = a.field == b.field

active_ :: forall b a r. Lens { active :: a | r } { active :: b | r } a b
active_ f o = o { active = _ } <$> f o.active


bigRecord_ :: forall b a r. Lens { bigRecord :: a | r } { bigRecord :: b | r } a b
bigRecord_ f o = o { bigRecord = _ } <$> f o.bigRecord


bool_ :: forall b a r. Lens { bool :: a | r } { bool :: b | r } a b
bool_ f o = o { bool = _ } <$> f o.bool


boom1_ :: forall b a r. Lens { boom1 :: a | r } { boom1 :: b | r } a b
boom1_ f o = o { boom1 = _ } <$> f o.boom1


classP_ :: forall b a r. Lens { classP :: a | r } { classP :: b | r } a b
classP_ f o = o { classP = _ } <$> f o.classP


createdAt_ :: forall b a r. Lens { createdAt :: a | r } { createdAt :: b | r } a b
createdAt_ f o = o { createdAt = _ } <$> f o.createdAt


dataP_ :: forall b a r. Lens { dataP :: a | r } { dataP :: b | r } a b
dataP_ f o = o { dataP = _ } <$> f o.dataP


email_ :: forall b a r. Lens { email :: a | r } { email :: b | r } a b
email_ f o = o { email = _ } <$> f o.email


field_ :: forall b a r. Lens { field :: a | r } { field :: b | r } a b
field_ f o = o { field = _ } <$> f o.field


id_ :: forall b a r. Lens { id :: a | r } { id :: b | r } a b
id_ f o = o { id = _ } <$> f o.id


int_ :: forall b a r. Lens { int :: a | r } { int :: b | r } a b
int_ f o = o { int = _ } <$> f o.int


integer_ :: forall b a r. Lens { integer :: a | r } { integer :: b | r } a b
integer_ f o = o { integer = _ } <$> f o.integer


letP_ :: forall b a r. Lens { letP :: a | r } { letP :: b | r } a b
letP_ f o = o { letP = _ } <$> f o.letP


maybeInt_ :: forall b a r. Lens { maybeInt :: a | r } { maybeInt :: b | r } a b
maybeInt_ f o = o { maybeInt = _ } <$> f o.maybeInt


maybeInteger_ :: forall b a r. Lens { maybeInteger :: a | r } { maybeInteger :: b | r } a b
maybeInteger_ f o = o { maybeInteger = _ } <$> f o.maybeInteger


modifiedAt_ :: forall b a r. Lens { modifiedAt :: a | r } { modifiedAt :: b | r } a b
modifiedAt_ f o = o { modifiedAt = _ } <$> f o.modifiedAt


moduleP_ :: forall b a r. Lens { moduleP :: a | r } { moduleP :: b | r } a b
moduleP_ f o = o { moduleP = _ } <$> f o.moduleP


name_ :: forall b a r. Lens { name :: a | r } { name :: b | r } a b
name_ f o = o { name = _ } <$> f o.name


string_ :: forall b a r. Lens { string :: a | r } { string :: b | r } a b
string_ f o = o { string = _ } <$> f o.string


string2_ :: forall b a r. Lens { string2 :: a | r } { string2 :: b | r } a b
string2_ f o = o { string2 = _ } <$> f o.string2


sumType_ :: forall b a r. Lens { sumType :: a | r } { sumType :: b | r } a b
sumType_ f o = o { sumType = _ } <$> f o.sumType


unSession_ :: forall b a r. Lens { unSession :: a | r } { unSession :: b | r } a b
unSession_ f o = o { unSession = _ } <$> f o.unSession


userResponses_ :: forall b a r. Lens { userResponses :: a | r } { userResponses :: b | r } a b
userResponses_ f o = o { userResponses = _ } <$> f o.userResponses

-- footer
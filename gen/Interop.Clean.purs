module Interop.Clean where


import Control.Monad.Aff
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Printer
import Data.Date.Helpers
import Data.Either
import Data.Foreign (unsafeFromForeign)
import Data.Foreign.Class
import Data.JSON
import Data.List (List ())
import Data.Maybe
import Data.Set (Set ())
import Data.Tuple
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Optic.Lens
import Optic.Core
import Prelude

newtype Session = Session {
  unSession :: String
}


_Session :: LensP Session {
  unSession :: String
}
_Session f (Session o) = Session <$> f o


mkSession :: String -> Session
mkSession unSession =
  Session{unSession}


unwrapSession (Session r) = r

instance sessionToJson :: ToJSON Session where
  toJSON (Session v) = object $
    [ "tag" .= "Session"
    , "un_session" .= v.unSession
    ]


instance sessionFromJSON :: FromJSON Session where
  parseJSON (JObject o) = do
    unSession <- o .: "un_session"
    return $ Session {
      unSession : unSession
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance sessionIsForeign :: IsForeign Session where
  read = unsafeFromForeign


instance sessionShow :: Show Session where
    show (Session o) = show "unSession: " ++ show o.unSession

instance sessionEq :: Eq Session where
  eq (Session a) (Session b) = a.unSession == b.unSession

data SumType
  = A 
  | B Int
  | C Boolean
  | D String
  | E (Array  Int)
  | F SumType
  | G (Array  SumType)
  | H Boolean Int String (Maybe Boolean)



instance sumTypeToJSON :: ToJSON SumType where
  toJSON (A ) = object $
    [ "tag" .= "A"
    , "contents" .= ([] :: Array String)
    ]
  toJSON (B x0) = object $
    [ "tag" .= "B"
    , "contents" .= toJSON x0
    ]
  toJSON (C x0) = object $
    [ "tag" .= "C"
    , "contents" .= toJSON x0
    ]
  toJSON (D x0) = object $
    [ "tag" .= "D"
    , "contents" .= toJSON x0
    ]
  toJSON (E x0) = object $
    [ "tag" .= "E"
    , "contents" .= toJSON x0
    ]
  toJSON (F x0) = object $
    [ "tag" .= "F"
    , "contents" .= toJSON x0
    ]
  toJSON (G x0) = object $
    [ "tag" .= "G"
    , "contents" .= toJSON x0
    ]
  toJSON (H x0 x1 x2 x3) = object $
    [ "tag" .= "H"
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]


instance sumTypeFromJSON :: FromJSON SumType where
  parseJSON (JObject o) = do
    tag <- o .: "tag"
    case tag of
      "A" -> do
        return $ A

      "B" -> do
        x0 <- o .: "contents"
        B <$> parseJSON x0

      "C" -> do
        x0 <- o .: "contents"
        C <$> parseJSON x0

      "D" -> do
        x0 <- o .: "contents"
        D <$> parseJSON x0

      "E" -> do
        x0 <- o .: "contents"
        E <$> parseJSON x0

      "F" -> do
        x0 <- o .: "contents"
        F <$> parseJSON x0

      "G" -> do
        x0 <- o .: "contents"
        G <$> parseJSON x0

      "H" -> do
        [x0, x1, x2, x3] <- o .: "contents"
        H <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3

  parseJSON x = fail $ "Could not parse object: " ++ show x


instance sumTypeEncodeJson :: EncodeJson SumType where
  encodeJson (A ) =
       "tag" := "A"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (B x0) =
       "tag" := "B"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (C x0) =
       "tag" := "C"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (D x0) =
       "tag" := "D"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (E x0) =
       "tag" := "E"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (F x0) =
       "tag" := "F"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (G x0) =
       "tag" := "G"
    ~> "contents" := encodeJson x0
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
          return $ A

        "B" -> do
          x0 <- obj .? "contents"
          B <$> decodeJson x0

        "C" -> do
          x0 <- obj .? "contents"
          C <$> decodeJson x0

        "D" -> do
          x0 <- obj .? "contents"
          D <$> decodeJson x0

        "E" -> do
          x0 <- obj .? "contents"
          E <$> decodeJson x0

        "F" -> do
          x0 <- obj .? "contents"
          F <$> decodeJson x0

        "G" -> do
          x0 <- obj .? "contents"
          G <$> decodeJson x0

        "H" -> do
          [x0, x1, x2, x3] <- obj .? "contents"
          H <$> decodeJson x0 <*> decodeJson x1 <*> decodeJson x2 <*> decodeJson x3

  decodeJson x = fail $ "Could not parse object: " ++ show x


instance sumTypeRequestable :: Requestable SumType where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sumTypeRespondable :: Respondable SumType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = unsafeFromForeign


instance sumTypeIsForeign :: IsForeign SumType where
  read = unsafeFromForeign


instance sumTypeShow :: Show SumType where
  show (A) = "A"
  show (B x0) = "B: " ++ show x0
  show (C x0) = "C: " ++ show x0
  show (D x0) = "D: " ++ show x0
  show (E x0) = "E: " ++ show x0
  show (F x0) = "F: " ++ show x0
  show (G x0) = "G: " ++ show x0
  show (H x0 x1 x2 x3) = "H: " ++ show x0 ++ " " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3


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
  string2 :: (Array  Char),
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}


_BigRecord :: LensP BigRecord {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  string2 :: (Array  Char),
  sumType :: SumType,
  dataP :: String,
  classP :: String,
  letP :: String,
  moduleP :: String,
  bigRecord :: Boolean
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> (Array  Char) -> SumType -> String -> String -> String -> String -> Boolean -> BigRecord
mkBigRecord bool int maybeInt integer maybeInteger string string2 sumType dataP classP letP moduleP bigRecord =
  BigRecord{bool, int, maybeInt, integer, maybeInteger, string, string2, sumType, dataP, classP, letP, moduleP, bigRecord}


unwrapBigRecord (BigRecord r) = r

instance bigRecordToJson :: ToJSON BigRecord where
  toJSON (BigRecord v) = object $
    [ "tag" .= "BigRecord"
    , "bool" .= v.bool
    , "int" .= v.int
    , "maybe_int" .= v.maybeInt
    , "integer" .= v.integer
    , "maybe_integer" .= v.maybeInteger
    , "string" .= v.string
    , "string2" .= v.string2
    , "sum_type" .= v.sumType
    , "data_p" .= v.dataP
    , "class_p" .= v.classP
    , "let_p" .= v.letP
    , "module_p" .= v.moduleP
    , "big_record" .= v.bigRecord
    ]


instance bigRecordFromJSON :: FromJSON BigRecord where
  parseJSON (JObject o) = do
    bool <- o .: "bool"
    int <- o .: "int"
    maybeInt <- o .: "maybe_int"
    integer <- o .: "integer"
    maybeInteger <- o .: "maybe_integer"
    string <- o .: "string"
    string2 <- o .: "string2"
    sumType <- o .: "sum_type"
    dataP <- o .: "data_p"
    classP <- o .: "class_p"
    letP <- o .: "let_p"
    moduleP <- o .: "module_p"
    bigRecord <- o .: "big_record"
    return $ BigRecord {
      bool : bool,
      int : int,
      maybeInt : maybeInt,
      integer : integer,
      maybeInteger : maybeInteger,
      string : string,
      string2 : string2,
      sumType : sumType,
      dataP : dataP,
      classP : classP,
      letP : letP,
      moduleP : moduleP,
      bigRecord : bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
    ~> "data_p" := o.dataP
    ~> "class_p" := o.classP
    ~> "let_p" := o.letP
    ~> "module_p" := o.moduleP
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
    dataP <- obj .? "data_p"
    classP <- obj .? "class_p"
    letP <- obj .? "let_p"
    moduleP <- obj .? "module_p"
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
  fromResponse = unsafeFromForeign


instance bigRecordIsForeign :: IsForeign BigRecord where
  read = unsafeFromForeign


instance bigRecordShow :: Show BigRecord where
    show (BigRecord o) = show "bool: " ++ show o.bool ++ ", " ++ show "int: " ++ show o.int ++ ", " ++ show "maybeInt: " ++ show o.maybeInt ++ ", " ++ show "integer: " ++ show o.integer ++ ", " ++ show "maybeInteger: " ++ show o.maybeInteger ++ ", " ++ show "string: " ++ show o.string ++ ", " ++ show "string2: " ++ show o.string2 ++ ", " ++ show "sumType: " ++ show o.sumType ++ ", " ++ show "dataP: " ++ show o.dataP ++ ", " ++ show "classP: " ++ show o.classP ++ ", " ++ show "letP: " ++ show o.letP ++ ", " ++ show "moduleP: " ++ show o.moduleP ++ ", " ++ show "bigRecord: " ++ show o.bigRecord

instance bigRecordEq :: Eq BigRecord where
  eq (BigRecord a) (BigRecord b) = a.bool == b.bool && a.int == b.int && a.maybeInt == b.maybeInt && a.integer == b.integer && a.maybeInteger == b.maybeInteger && a.string == b.string && a.string2 == b.string2 && a.sumType == b.sumType && a.dataP == b.dataP && a.classP == b.classP && a.letP == b.letP && a.moduleP == b.moduleP && a.bigRecord == b.bigRecord

type FakeUTCTime = Int


newtype User = User {
  name :: String,
  email :: String,
  active :: Boolean
}


_User :: LensP User {
  name :: String,
  email :: String,
  active :: Boolean
}
_User f (User o) = User <$> f o


mkUser :: String -> String -> Boolean -> User
mkUser name email active =
  User{name, email, active}


unwrapUser (User r) = r

instance userToJson :: ToJSON User where
  toJSON (User v) = object $
    [ "tag" .= "User"
    , "name" .= v.name
    , "email" .= v.email
    , "active" .= v.active
    ]


instance userFromJSON :: FromJSON User where
  parseJSON (JObject o) = do
    name <- o .: "name"
    email <- o .: "email"
    active <- o .: "active"
    return $ User {
      name : name,
      email : email,
      active : active
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userIsForeign :: IsForeign User where
  read = unsafeFromForeign


instance userShow :: Show User where
    show (User o) = show "name: " ++ show o.name ++ ", " ++ show "email: " ++ show o.email ++ ", " ++ show "active: " ++ show o.active

instance userEq :: Eq User where
  eq (User a) (User b) = a.name == b.name && a.email == b.email && a.active == b.active

newtype UserRequest = UserRequest {
  name :: String,
  email :: String
}


_UserRequest :: LensP UserRequest {
  name :: String,
  email :: String
}
_UserRequest f (UserRequest o) = UserRequest <$> f o


mkUserRequest :: String -> String -> UserRequest
mkUserRequest name email =
  UserRequest{name, email}


unwrapUserRequest (UserRequest r) = r

instance userRequestToJson :: ToJSON UserRequest where
  toJSON (UserRequest v) = object $
    [ "tag" .= "UserRequest"
    , "name" .= v.name
    , "email" .= v.email
    ]


instance userRequestFromJSON :: FromJSON UserRequest where
  parseJSON (JObject o) = do
    name <- o .: "name"
    email <- o .: "email"
    return $ UserRequest {
      name : name,
      email : email
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userRequestIsForeign :: IsForeign UserRequest where
  read = unsafeFromForeign


instance userRequestShow :: Show UserRequest where
    show (UserRequest o) = show "name: " ++ show o.name ++ ", " ++ show "email: " ++ show o.email

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


_UserResponse :: LensP UserResponse {
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


unwrapUserResponse (UserResponse r) = r

instance userResponseToJson :: ToJSON UserResponse where
  toJSON (UserResponse v) = object $
    [ "tag" .= "UserResponse"
    , "id" .= v.id
    , "name" .= v.name
    , "email" .= v.email
    , "active" .= v.active
    , "created_at" .= v.createdAt
    , "modified_at" .= v.modifiedAt
    ]


instance userResponseFromJSON :: FromJSON UserResponse where
  parseJSON (JObject o) = do
    id <- o .: "id"
    name <- o .: "name"
    email <- o .: "email"
    active <- o .: "active"
    createdAt <- o .: "created_at"
    modifiedAt <- o .: "modified_at"
    return $ UserResponse {
      id : id,
      name : name,
      email : email,
      active : active,
      createdAt : createdAt,
      modifiedAt : modifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userResponseIsForeign :: IsForeign UserResponse where
  read = unsafeFromForeign


instance userResponseShow :: Show UserResponse where
    show (UserResponse o) = show "id: " ++ show o.id ++ ", " ++ show "name: " ++ show o.name ++ ", " ++ show "email: " ++ show o.email ++ ", " ++ show "active: " ++ show o.active ++ ", " ++ show "createdAt: " ++ show o.createdAt ++ ", " ++ show "modifiedAt: " ++ show o.modifiedAt

instance userResponseEq :: Eq UserResponse where
  eq (UserResponse a) (UserResponse b) = a.id == b.id && a.name == b.name && a.email == b.email && a.active == b.active && a.createdAt == b.createdAt && a.modifiedAt == b.modifiedAt

type Text = String


type TextMaybe = (Maybe String)


newtype FunkyRecord = Boom1 {
  boom1 :: Boolean
}


_FunkyRecord :: LensP FunkyRecord {
  boom1 :: Boolean
}
_FunkyRecord f (Boom1 o) = Boom1 <$> f o


mkFunkyRecord :: Boolean -> FunkyRecord
mkFunkyRecord boom1 =
  Boom1{boom1}


unwrapFunkyRecord (Boom1 r) = r

instance funkyRecordToJson :: ToJSON FunkyRecord where
  toJSON (Boom1 v) = object $
    [ "tag" .= "FunkyRecord"
    , "boom1" .= v.boom1
    ]


instance funkyRecordFromJSON :: FromJSON FunkyRecord where
  parseJSON (JObject o) = do
    boom1 <- o .: "boom1"
    return $ Boom1 {
      boom1 : boom1
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance funkyRecordIsForeign :: IsForeign FunkyRecord where
  read = unsafeFromForeign


instance funkyRecordShow :: Show FunkyRecord where
    show (Boom1 o) = show "boom1: " ++ show o.boom1

instance funkyRecordEq :: Eq FunkyRecord where
  eq (Boom1 a) (Boom1 b) = a.boom1 == b.boom1

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

-- footer
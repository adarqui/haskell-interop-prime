module Interop where


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
    , "unSession" .= v.unSession
    ]


instance sessionFromJSON :: FromJSON Session where
  parseJSON (JObject o) = do
    unSession <- o .: "unSession"
    return $ Session {
      unSession : unSession
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: (Array  Char),
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}


_BigRecord :: LensP BigRecord {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordString2 :: (Array  Char),
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> (Array  Char) -> SumType -> String -> String -> String -> String -> Boolean -> BigRecord
mkBigRecord bigRecordBool bigRecordInt bigRecordMaybeInt bigRecordInteger bigRecordMaybeInteger bigRecordString bigRecordString2 bigRecordSumType bigRecordData bigRecordClass bigRecordLet bigRecordModule bigRecord =
  BigRecord{bigRecordBool, bigRecordInt, bigRecordMaybeInt, bigRecordInteger, bigRecordMaybeInteger, bigRecordString, bigRecordString2, bigRecordSumType, bigRecordData, bigRecordClass, bigRecordLet, bigRecordModule, bigRecord}


unwrapBigRecord (BigRecord r) = r

instance bigRecordToJson :: ToJSON BigRecord where
  toJSON (BigRecord v) = object $
    [ "tag" .= "BigRecord"
    , "bigRecordBool" .= v.bigRecordBool
    , "bigRecordInt" .= v.bigRecordInt
    , "bigRecordMaybeInt" .= v.bigRecordMaybeInt
    , "bigRecordInteger" .= v.bigRecordInteger
    , "bigRecordMaybeInteger" .= v.bigRecordMaybeInteger
    , "bigRecordString" .= v.bigRecordString
    , "bigRecordString2" .= v.bigRecordString2
    , "bigRecordSumType" .= v.bigRecordSumType
    , "bigRecordData" .= v.bigRecordData
    , "bigRecordClass" .= v.bigRecordClass
    , "bigRecordLet" .= v.bigRecordLet
    , "bigRecordModule" .= v.bigRecordModule
    , "bigRecord" .= v.bigRecord
    ]


instance bigRecordFromJSON :: FromJSON BigRecord where
  parseJSON (JObject o) = do
    bigRecordBool <- o .: "bigRecordBool"
    bigRecordInt <- o .: "bigRecordInt"
    bigRecordMaybeInt <- o .: "bigRecordMaybeInt"
    bigRecordInteger <- o .: "bigRecordInteger"
    bigRecordMaybeInteger <- o .: "bigRecordMaybeInteger"
    bigRecordString <- o .: "bigRecordString"
    bigRecordString2 <- o .: "bigRecordString2"
    bigRecordSumType <- o .: "bigRecordSumType"
    bigRecordData <- o .: "bigRecordData"
    bigRecordClass <- o .: "bigRecordClass"
    bigRecordLet <- o .: "bigRecordLet"
    bigRecordModule <- o .: "bigRecordModule"
    bigRecord <- o .: "bigRecord"
    return $ BigRecord {
      bigRecordBool : bigRecordBool,
      bigRecordInt : bigRecordInt,
      bigRecordMaybeInt : bigRecordMaybeInt,
      bigRecordInteger : bigRecordInteger,
      bigRecordMaybeInteger : bigRecordMaybeInteger,
      bigRecordString : bigRecordString,
      bigRecordString2 : bigRecordString2,
      bigRecordSumType : bigRecordSumType,
      bigRecordData : bigRecordData,
      bigRecordClass : bigRecordClass,
      bigRecordLet : bigRecordLet,
      bigRecordModule : bigRecordModule,
      bigRecord : bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance bigRecordIsForeign :: IsForeign BigRecord where
  read = unsafeFromForeign


instance bigRecordShow :: Show BigRecord where
    show (BigRecord o) = show "bigRecordBool: " ++ show o.bigRecordBool ++ ", " ++ show "bigRecordInt: " ++ show o.bigRecordInt ++ ", " ++ show "bigRecordMaybeInt: " ++ show o.bigRecordMaybeInt ++ ", " ++ show "bigRecordInteger: " ++ show o.bigRecordInteger ++ ", " ++ show "bigRecordMaybeInteger: " ++ show o.bigRecordMaybeInteger ++ ", " ++ show "bigRecordString: " ++ show o.bigRecordString ++ ", " ++ show "bigRecordString2: " ++ show o.bigRecordString2 ++ ", " ++ show "bigRecordSumType: " ++ show o.bigRecordSumType ++ ", " ++ show "bigRecordData: " ++ show o.bigRecordData ++ ", " ++ show "bigRecordClass: " ++ show o.bigRecordClass ++ ", " ++ show "bigRecordLet: " ++ show o.bigRecordLet ++ ", " ++ show "bigRecordModule: " ++ show o.bigRecordModule ++ ", " ++ show "bigRecord: " ++ show o.bigRecord

instance bigRecordEq :: Eq BigRecord where
  eq (BigRecord a) (BigRecord b) = a.bigRecordBool == b.bigRecordBool && a.bigRecordInt == b.bigRecordInt && a.bigRecordMaybeInt == b.bigRecordMaybeInt && a.bigRecordInteger == b.bigRecordInteger && a.bigRecordMaybeInteger == b.bigRecordMaybeInteger && a.bigRecordString == b.bigRecordString && a.bigRecordString2 == b.bigRecordString2 && a.bigRecordSumType == b.bigRecordSumType && a.bigRecordData == b.bigRecordData && a.bigRecordClass == b.bigRecordClass && a.bigRecordLet == b.bigRecordLet && a.bigRecordModule == b.bigRecordModule && a.bigRecord == b.bigRecord

type FakeUTCTime = Int


newtype User = User {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}


_User :: LensP User {
  userName :: String,
  userEmail :: String,
  userActive :: Boolean
}
_User f (User o) = User <$> f o


mkUser :: String -> String -> Boolean -> User
mkUser userName userEmail userActive =
  User{userName, userEmail, userActive}


unwrapUser (User r) = r

instance userToJson :: ToJSON User where
  toJSON (User v) = object $
    [ "tag" .= "User"
    , "userName" .= v.userName
    , "userEmail" .= v.userEmail
    , "userActive" .= v.userActive
    ]


instance userFromJSON :: FromJSON User where
  parseJSON (JObject o) = do
    userName <- o .: "userName"
    userEmail <- o .: "userEmail"
    userActive <- o .: "userActive"
    return $ User {
      userName : userName,
      userEmail : userEmail,
      userActive : userActive
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userIsForeign :: IsForeign User where
  read = unsafeFromForeign


instance userShow :: Show User where
    show (User o) = show "userName: " ++ show o.userName ++ ", " ++ show "userEmail: " ++ show o.userEmail ++ ", " ++ show "userActive: " ++ show o.userActive

instance userEq :: Eq User where
  eq (User a) (User b) = a.userName == b.userName && a.userEmail == b.userEmail && a.userActive == b.userActive

newtype UserRequest = UserRequest {
  userRequestName :: String,
  userRequestEmail :: String
}


_UserRequest :: LensP UserRequest {
  userRequestName :: String,
  userRequestEmail :: String
}
_UserRequest f (UserRequest o) = UserRequest <$> f o


mkUserRequest :: String -> String -> UserRequest
mkUserRequest userRequestName userRequestEmail =
  UserRequest{userRequestName, userRequestEmail}


unwrapUserRequest (UserRequest r) = r

instance userRequestToJson :: ToJSON UserRequest where
  toJSON (UserRequest v) = object $
    [ "tag" .= "UserRequest"
    , "userRequestName" .= v.userRequestName
    , "userRequestEmail" .= v.userRequestEmail
    ]


instance userRequestFromJSON :: FromJSON UserRequest where
  parseJSON (JObject o) = do
    userRequestName <- o .: "userRequestName"
    userRequestEmail <- o .: "userRequestEmail"
    return $ UserRequest {
      userRequestName : userRequestName,
      userRequestEmail : userRequestEmail
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userRequestIsForeign :: IsForeign UserRequest where
  read = unsafeFromForeign


instance userRequestShow :: Show UserRequest where
    show (UserRequest o) = show "userRequestName: " ++ show o.userRequestName ++ ", " ++ show "userRequestEmail: " ++ show o.userRequestEmail

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


_UserResponse :: LensP UserResponse {
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


unwrapUserResponse (UserResponse r) = r

instance userResponseToJson :: ToJSON UserResponse where
  toJSON (UserResponse v) = object $
    [ "tag" .= "UserResponse"
    , "userResponseId" .= v.userResponseId
    , "userResponseName" .= v.userResponseName
    , "userResponseEmail" .= v.userResponseEmail
    , "userResponseActive" .= v.userResponseActive
    , "userResponseCreatedAt" .= v.userResponseCreatedAt
    , "userResponseModifiedAt" .= v.userResponseModifiedAt
    ]


instance userResponseFromJSON :: FromJSON UserResponse where
  parseJSON (JObject o) = do
    userResponseId <- o .: "userResponseId"
    userResponseName <- o .: "userResponseName"
    userResponseEmail <- o .: "userResponseEmail"
    userResponseActive <- o .: "userResponseActive"
    userResponseCreatedAt <- o .: "userResponseCreatedAt"
    userResponseModifiedAt <- o .: "userResponseModifiedAt"
    return $ UserResponse {
      userResponseId : userResponseId,
      userResponseName : userResponseName,
      userResponseEmail : userResponseEmail,
      userResponseActive : userResponseActive,
      userResponseCreatedAt : userResponseCreatedAt,
      userResponseModifiedAt : userResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
  fromResponse = unsafeFromForeign


instance userResponseIsForeign :: IsForeign UserResponse where
  read = unsafeFromForeign


instance userResponseShow :: Show UserResponse where
    show (UserResponse o) = show "userResponseId: " ++ show o.userResponseId ++ ", " ++ show "userResponseName: " ++ show o.userResponseName ++ ", " ++ show "userResponseEmail: " ++ show o.userResponseEmail ++ ", " ++ show "userResponseActive: " ++ show o.userResponseActive ++ ", " ++ show "userResponseCreatedAt: " ++ show o.userResponseCreatedAt ++ ", " ++ show "userResponseModifiedAt: " ++ show o.userResponseModifiedAt

instance userResponseEq :: Eq UserResponse where
  eq (UserResponse a) (UserResponse b) = a.userResponseId == b.userResponseId && a.userResponseName == b.userResponseName && a.userResponseEmail == b.userResponseEmail && a.userResponseActive == b.userResponseActive && a.userResponseCreatedAt == b.userResponseCreatedAt && a.userResponseModifiedAt == b.userResponseModifiedAt

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

-- footer
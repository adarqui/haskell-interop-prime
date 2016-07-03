{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= ("Session" :: Text)
    , "unSession" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: ("unSession" :: Text)
    pure $ Session {
      unSession = unSession
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show Session where
    show rec = show "unSession: " <> show (unSession rec)

instance Eq Session where
  (==) a b = unSession a == unSession b

instance ToJSON SumType where
  toJSON (A ) = object $
    [ "tag" .= ("A" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (B x0) = object $
    [ "tag" .= ("B" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (C x0) = object $
    [ "tag" .= ("C" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (D x0) = object $
    [ "tag" .= ("D" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (E x0) = object $
    [ "tag" .= ("E" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (F x0) = object $
    [ "tag" .= ("F" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (G x0) = object $
    [ "tag" .= ("G" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (H x0 x1 x2 x3) = object $
    [ "tag" .= ("H" :: Text)
    , "contents" .= [toJSON x0, toJSON x1, toJSON x2, toJSON x3]
    ]


instance FromJSON SumType where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("A" :: Text) -> do
        pure A

      ("B" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> B <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: B"

      ("C" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> C <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: C"

      ("D" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> D <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: D"

      ("E" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> E <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: E"

      ("F" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> F <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: F"

      ("G" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> G <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: G"

      ("H" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1, x2, x3] -> H <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3
          _ -> fail "FromJON Typemismatch: H"

      _ -> fail "Could not parse SumType"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show SumType where
  show A = "A"
  show (B x0) = "B: " <> show x0
  show (C x0) = "C: " <> show x0
  show (D x0) = "D: " <> show x0
  show (E x0) = "E: " <> show x0
  show (F x0) = "F: " <> show x0
  show (G x0) = "G: " <> show x0
  show (H x0 x1 x2 x3) = "H: " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3


instance Eq SumType where
  (==) A A = True
  (==) (B x0a) (B x0b) = x0a == x0b
  (==) (C x0a) (C x0b) = x0a == x0b
  (==) (D x0a) (D x0b) = x0a == x0b
  (==) (E x0a) (E x0b) = x0a == x0b
  (==) (F x0a) (F x0b) = x0a == x0b
  (==) (G x0a) (G x0b) = x0a == x0b
  (==) (H x0a x1a x2a x3a) (H x0b x1b x2b x3b) = x0a == x0b && x1a == x1b && x2a == x2b && x3a == x3b
  (==) _ _ = False

instance ToJSON BigRecord where
  toJSON BigRecord{..} = object $
    [ "tag" .= ("BigRecord" :: Text)
    , "bigRecordBool" .= bigRecordBool
    , "bigRecordInt" .= bigRecordInt
    , "bigRecordMaybeInt" .= bigRecordMaybeInt
    , "bigRecordInteger" .= bigRecordInteger
    , "bigRecordMaybeInteger" .= bigRecordMaybeInteger
    , "bigRecordString" .= bigRecordString
    , "bigRecordString2" .= bigRecordString2
    , "bigRecordSumType" .= bigRecordSumType
    , "bigRecordData" .= bigRecordData
    , "bigRecordClass" .= bigRecordClass
    , "bigRecordLet" .= bigRecordLet
    , "bigRecordModule" .= bigRecordModule
    , "bigRecord" .= bigRecord
    ]


instance FromJSON BigRecord where
  parseJSON (Object o) = do
    bigRecordBool <- o .: ("bigRecordBool" :: Text)
    bigRecordInt <- o .: ("bigRecordInt" :: Text)
    bigRecordMaybeInt <- o .: ("bigRecordMaybeInt" :: Text)
    bigRecordInteger <- o .: ("bigRecordInteger" :: Text)
    bigRecordMaybeInteger <- o .: ("bigRecordMaybeInteger" :: Text)
    bigRecordString <- o .: ("bigRecordString" :: Text)
    bigRecordString2 <- o .: ("bigRecordString2" :: Text)
    bigRecordSumType <- o .: ("bigRecordSumType" :: Text)
    bigRecordData <- o .: ("bigRecordData" :: Text)
    bigRecordClass <- o .: ("bigRecordClass" :: Text)
    bigRecordLet <- o .: ("bigRecordLet" :: Text)
    bigRecordModule <- o .: ("bigRecordModule" :: Text)
    bigRecord <- o .: ("bigRecord" :: Text)
    pure $ BigRecord {
      bigRecordBool = bigRecordBool,
      bigRecordInt = bigRecordInt,
      bigRecordMaybeInt = bigRecordMaybeInt,
      bigRecordInteger = bigRecordInteger,
      bigRecordMaybeInteger = bigRecordMaybeInteger,
      bigRecordString = bigRecordString,
      bigRecordString2 = bigRecordString2,
      bigRecordSumType = bigRecordSumType,
      bigRecordData = bigRecordData,
      bigRecordClass = bigRecordClass,
      bigRecordLet = bigRecordLet,
      bigRecordModule = bigRecordModule,
      bigRecord = bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show BigRecord where
    show rec = show "bigRecordBool: " <> show (bigRecordBool rec) <> ", " <> show "bigRecordInt: " <> show (bigRecordInt rec) <> ", " <> show "bigRecordMaybeInt: " <> show (bigRecordMaybeInt rec) <> ", " <> show "bigRecordInteger: " <> show (bigRecordInteger rec) <> ", " <> show "bigRecordMaybeInteger: " <> show (bigRecordMaybeInteger rec) <> ", " <> show "bigRecordString: " <> show (bigRecordString rec) <> ", " <> show "bigRecordString2: " <> show (bigRecordString2 rec) <> ", " <> show "bigRecordSumType: " <> show (bigRecordSumType rec) <> ", " <> show "bigRecordData: " <> show (bigRecordData rec) <> ", " <> show "bigRecordClass: " <> show (bigRecordClass rec) <> ", " <> show "bigRecordLet: " <> show (bigRecordLet rec) <> ", " <> show "bigRecordModule: " <> show (bigRecordModule rec) <> ", " <> show "bigRecord: " <> show (bigRecord rec)

instance Eq BigRecord where
  (==) a b = bigRecordBool a == bigRecordBool b && bigRecordInt a == bigRecordInt b && bigRecordMaybeInt a == bigRecordMaybeInt b && bigRecordInteger a == bigRecordInteger b && bigRecordMaybeInteger a == bigRecordMaybeInteger b && bigRecordString a == bigRecordString b && bigRecordString2 a == bigRecordString2 b && bigRecordSumType a == bigRecordSumType b && bigRecordData a == bigRecordData b && bigRecordClass a == bigRecordClass b && bigRecordLet a == bigRecordLet b && bigRecordModule a == bigRecordModule b && bigRecord a == bigRecord b

instance ToJSON User where
  toJSON User{..} = object $
    [ "tag" .= ("User" :: Text)
    , "userName" .= userName
    , "userEmail" .= userEmail
    , "userActive" .= userActive
    ]


instance FromJSON User where
  parseJSON (Object o) = do
    userName <- o .: ("userName" :: Text)
    userEmail <- o .: ("userEmail" :: Text)
    userActive <- o .: ("userActive" :: Text)
    pure $ User {
      userName = userName,
      userEmail = userEmail,
      userActive = userActive
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show User where
    show rec = show "userName: " <> show (userName rec) <> ", " <> show "userEmail: " <> show (userEmail rec) <> ", " <> show "userActive: " <> show (userActive rec)

instance Eq User where
  (==) a b = userName a == userName b && userEmail a == userEmail b && userActive a == userActive b

instance ToJSON UserRequest where
  toJSON UserRequest{..} = object $
    [ "tag" .= ("UserRequest" :: Text)
    , "userRequestName" .= userRequestName
    , "userRequestEmail" .= userRequestEmail
    ]


instance FromJSON UserRequest where
  parseJSON (Object o) = do
    userRequestName <- o .: ("userRequestName" :: Text)
    userRequestEmail <- o .: ("userRequestEmail" :: Text)
    pure $ UserRequest {
      userRequestName = userRequestName,
      userRequestEmail = userRequestEmail
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show UserRequest where
    show rec = show "userRequestName: " <> show (userRequestName rec) <> ", " <> show "userRequestEmail: " <> show (userRequestEmail rec)

instance Eq UserRequest where
  (==) a b = userRequestName a == userRequestName b && userRequestEmail a == userRequestEmail b

instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= ("UserResponse" :: Text)
    , "userResponseId" .= userResponseId
    , "userResponseName" .= userResponseName
    , "userResponseEmail" .= userResponseEmail
    , "userResponseActive" .= userResponseActive
    , "userResponseCreatedAt" .= userResponseCreatedAt
    , "userResponseModifiedAt" .= userResponseModifiedAt
    ]


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    userResponseId <- o .: ("userResponseId" :: Text)
    userResponseName <- o .: ("userResponseName" :: Text)
    userResponseEmail <- o .: ("userResponseEmail" :: Text)
    userResponseActive <- o .: ("userResponseActive" :: Text)
    userResponseCreatedAt <- o .: ("userResponseCreatedAt" :: Text)
    userResponseModifiedAt <- o .: ("userResponseModifiedAt" :: Text)
    pure $ UserResponse {
      userResponseId = userResponseId,
      userResponseName = userResponseName,
      userResponseEmail = userResponseEmail,
      userResponseActive = userResponseActive,
      userResponseCreatedAt = userResponseCreatedAt,
      userResponseModifiedAt = userResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show UserResponse where
    show rec = show "userResponseId: " <> show (userResponseId rec) <> ", " <> show "userResponseName: " <> show (userResponseName rec) <> ", " <> show "userResponseEmail: " <> show (userResponseEmail rec) <> ", " <> show "userResponseActive: " <> show (userResponseActive rec) <> ", " <> show "userResponseCreatedAt: " <> show (userResponseCreatedAt rec) <> ", " <> show "userResponseModifiedAt: " <> show (userResponseModifiedAt rec)

instance Eq UserResponse where
  (==) a b = userResponseId a == userResponseId b && userResponseName a == userResponseName b && userResponseEmail a == userResponseEmail b && userResponseActive a == userResponseActive b && userResponseCreatedAt a == userResponseCreatedAt b && userResponseModifiedAt a == userResponseModifiedAt b

instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= ("UserResponses" :: Text)
    , "userResponses" .= userResponses
    ]


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: ("userResponses" :: Text)
    pure $ UserResponses {
      userResponses = userResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show UserResponses where
    show rec = show "userResponses: " <> show (userResponses rec)

instance Eq UserResponses where
  (==) a b = userResponses a == userResponses b

instance ToJSON FunkyRecord where
  toJSON Boom1{..} = object $
    [ "tag" .= ("FunkyRecord" :: Text)
    , "boom1" .= boom1
    ]


instance FromJSON FunkyRecord where
  parseJSON (Object o) = do
    boom1 <- o .: ("boom1" :: Text)
    pure $ Boom1 {
      boom1 = boom1
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show FunkyRecord where
    show rec = show "boom1: " <> show (boom1 rec)

instance Eq FunkyRecord where
  (==) a b = boom1 a == boom1 b

instance ToJSON FUnkyRecordP where
  toJSON FUnkyRecordP{..} = object $
    [ "tag" .= ("FUnkyRecordP" :: Text)
    , "funkyrecordpField" .= funkyrecordpField
    ]


instance FromJSON FUnkyRecordP where
  parseJSON (Object o) = do
    funkyrecordpField <- o .: ("funkyrecordpField" :: Text)
    pure $ FUnkyRecordP {
      funkyrecordpField = funkyrecordpField
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show FUnkyRecordP where
    show rec = show "funkyrecordpField: " <> show (funkyrecordpField rec)

instance Eq FUnkyRecordP where
  (==) a b = funkyrecordpField a == funkyrecordpField b

instance ToJSON Param where
  toJSON (Limit x0) = object $
    [ "tag" .= ("Limit" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Offset x0) = object $
    [ "tag" .= ("Offset" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUsersIds x0) = object $
    [ "tag" .= ("ByUsersIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUserNameText x0) = object $
    [ "tag" .= ("ByUserNameText" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUserNameStr x0) = object $
    [ "tag" .= ("ByUserNameStr" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUsersNames x0) = object $
    [ "tag" .= ("ByUsersNames" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUsersEmails x0) = object $
    [ "tag" .= ("ByUsersEmails" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUserActive x0) = object $
    [ "tag" .= ("ByUserActive" :: Text)
    , "contents" .= [toJSON x0]
    ]


instance FromJSON Param where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Limit" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Limit <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Limit"

      ("Offset" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Offset <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Offset"

      ("ByUsersIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUsersIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUsersIds"

      ("ByUserNameText" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUserNameText <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUserNameText"

      ("ByUserNameStr" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUserNameStr <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUserNameStr"

      ("ByUsersNames" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUsersNames <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUsersNames"

      ("ByUsersEmails" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUsersEmails <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUsersEmails"

      ("ByUserActive" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUserActive <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUserActive"

      _ -> fail "Could not parse Param"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show Param where
  show (Limit x0) = "Limit: " <> show x0
  show (Offset x0) = "Offset: " <> show x0
  show (ByUsersIds x0) = "ByUsersIds: " <> show x0
  show (ByUserNameText x0) = "ByUserNameText: " <> show x0
  show (ByUserNameStr x0) = "ByUserNameStr: " <> show x0
  show (ByUsersNames x0) = "ByUsersNames: " <> show x0
  show (ByUsersEmails x0) = "ByUsersEmails: " <> show x0
  show (ByUserActive x0) = "ByUserActive: " <> show x0


instance Eq Param where
  (==) (Limit x0a) (Limit x0b) = x0a == x0b
  (==) (Offset x0a) (Offset x0b) = x0a == x0b
  (==) (ByUsersIds x0a) (ByUsersIds x0b) = x0a == x0b
  (==) (ByUserNameText x0a) (ByUserNameText x0b) = x0a == x0b
  (==) (ByUserNameStr x0a) (ByUserNameStr x0b) = x0a == x0b
  (==) (ByUsersNames x0a) (ByUsersNames x0b) = x0a == x0b
  (==) (ByUsersEmails x0a) (ByUsersEmails x0b) = x0a == x0b
  (==) (ByUserActive x0a) (ByUserActive x0b) = x0a == x0b
  (==) _ _ = False

instance QueryParam Param where
  qp (Limit x0) = ("Limit", (T.pack $ show x0))
  qp (Offset x0) = ("Offset", (T.pack $ show x0))
  qp (ByUsersIds x0) = ("ByUsersIds", (T.pack $ show x0))
  qp (ByUserNameText x0) = ("ByUserNameText", x0)
  qp (ByUserNameStr x0) = ("ByUserNameStr", (T.pack x0))
  qp (ByUsersNames x0) = ("ByUsersNames", (T.pack $ show x0))
  qp (ByUsersEmails x0) = ("ByUsersEmails", (T.pack $ show x0))
  qp (ByUserActive x0) = ("ByUserActive", (T.pack $ show x0))


instance ToJSON ParamTag where
  toJSON (ParamTag_Limit ) = object $
    [ "tag" .= ("ParamTag_Limit" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_Offset ) = object $
    [ "tag" .= ("ParamTag_Offset" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUsersIds ) = object $
    [ "tag" .= ("ParamTag_ByUsersIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUsersNames ) = object $
    [ "tag" .= ("ParamTag_ByUsersNames" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUsersEmails ) = object $
    [ "tag" .= ("ParamTag_ByUsersEmails" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUserActive ) = object $
    [ "tag" .= ("ParamTag_ByUserActive" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance FromJSON ParamTag where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("ParamTag_Limit" :: Text) -> do
        pure ParamTag_Limit

      ("ParamTag_Offset" :: Text) -> do
        pure ParamTag_Offset

      ("ParamTag_ByUsersIds" :: Text) -> do
        pure ParamTag_ByUsersIds

      ("ParamTag_ByUsersNames" :: Text) -> do
        pure ParamTag_ByUsersNames

      ("ParamTag_ByUsersEmails" :: Text) -> do
        pure ParamTag_ByUsersEmails

      ("ParamTag_ByUserActive" :: Text) -> do
        pure ParamTag_ByUserActive

      _ -> fail "Could not parse ParamTag"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show ParamTag where
  show ParamTag_Limit = "ParamTag_Limit"
  show ParamTag_Offset = "ParamTag_Offset"
  show ParamTag_ByUsersIds = "ParamTag_ByUsersIds"
  show ParamTag_ByUsersNames = "ParamTag_ByUsersNames"
  show ParamTag_ByUsersEmails = "ParamTag_ByUsersEmails"
  show ParamTag_ByUserActive = "ParamTag_ByUserActive"


instance Read ParamTag where
  readsPrec _ "ParamTag_Limit" = [(ParamTag_Limit, "")]
  readsPrec _ "ParamTag_Offset" = [(ParamTag_Offset, "")]
  readsPrec _ "ParamTag_ByUsersIds" = [(ParamTag_ByUsersIds, "")]
  readsPrec _ "ParamTag_ByUsersNames" = [(ParamTag_ByUsersNames, "")]
  readsPrec _ "ParamTag_ByUsersEmails" = [(ParamTag_ByUsersEmails, "")]
  readsPrec _ "ParamTag_ByUserActive" = [(ParamTag_ByUserActive, "")]
  readsPrec _ _ = []


instance Eq ParamTag where
  (==) ParamTag_Limit ParamTag_Limit = True
  (==) ParamTag_Offset ParamTag_Offset = True
  (==) ParamTag_ByUsersIds ParamTag_ByUsersIds = True
  (==) ParamTag_ByUsersNames ParamTag_ByUsersNames = True
  (==) ParamTag_ByUsersEmails ParamTag_ByUsersEmails = True
  (==) ParamTag_ByUserActive ParamTag_ByUserActive = True
  (==) _ _ = False
-- footer
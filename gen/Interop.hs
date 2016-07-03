{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop where




import Data.Aeson  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import Data.Text   (Text)
import Data.Monoid ((<>))

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
-- footer
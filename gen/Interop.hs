{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop where




import Data.Aeson
import Data.Text   (Text)
import Data.Monoid ((<>))

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= "Session"
    , "unSession" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: "unSession"
    pure $ Session {
      unSession = unSession
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON SumType where
  toJSON (A ) = object $
    [ "tag" .= "A"
    , "contents" .= ([] :: [Text])
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


instance FromJSON SumType where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag of
      "A" -> do
        pure A

      "B" -> do
        r <- o .: "contents"
        case r of
          x0 -> B <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: B"

      "C" -> do
        r <- o .: "contents"
        case r of
          x0 -> C <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: C"

      "D" -> do
        r <- o .: "contents"
        case r of
          x0 -> D <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: D"

      "E" -> do
        r <- o .: "contents"
        case r of
          x0 -> E <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: E"

      "F" -> do
        r <- o .: "contents"
        case r of
          x0 -> F <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: F"

      "G" -> do
        r <- o .: "contents"
        case r of
          x0 -> G <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: G"

      "H" -> do
        r <- o .: "contents"
        case r of
          [x0, x1, x2, x3] -> H <$> parseJSON x0 <*> parseJSON x1 <*> parseJSON x2 <*> parseJSON x3
          _ -> fail "FromJON Typemismatch: H"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BigRecord where
  toJSON BigRecord{..} = object $
    [ "tag" .= "BigRecord"
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


instance ToJSON User where
  toJSON User{..} = object $
    [ "tag" .= "User"
    , "userName" .= userName
    , "userEmail" .= userEmail
    , "userActive" .= userActive
    ]


instance FromJSON User where
  parseJSON (Object o) = do
    userName <- o .: "userName"
    userEmail <- o .: "userEmail"
    userActive <- o .: "userActive"
    pure $ User {
      userName = userName,
      userEmail = userEmail,
      userActive = userActive
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserRequest where
  toJSON UserRequest{..} = object $
    [ "tag" .= "UserRequest"
    , "userRequestName" .= userRequestName
    , "userRequestEmail" .= userRequestEmail
    ]


instance FromJSON UserRequest where
  parseJSON (Object o) = do
    userRequestName <- o .: "userRequestName"
    userRequestEmail <- o .: "userRequestEmail"
    pure $ UserRequest {
      userRequestName = userRequestName,
      userRequestEmail = userRequestEmail
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= "UserResponse"
    , "userResponseId" .= userResponseId
    , "userResponseName" .= userResponseName
    , "userResponseEmail" .= userResponseEmail
    , "userResponseActive" .= userResponseActive
    , "userResponseCreatedAt" .= userResponseCreatedAt
    , "userResponseModifiedAt" .= userResponseModifiedAt
    ]


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    userResponseId <- o .: "userResponseId"
    userResponseName <- o .: "userResponseName"
    userResponseEmail <- o .: "userResponseEmail"
    userResponseActive <- o .: "userResponseActive"
    userResponseCreatedAt <- o .: "userResponseCreatedAt"
    userResponseModifiedAt <- o .: "userResponseModifiedAt"
    pure $ UserResponse {
      userResponseId = userResponseId,
      userResponseName = userResponseName,
      userResponseEmail = userResponseEmail,
      userResponseActive = userResponseActive,
      userResponseCreatedAt = userResponseCreatedAt,
      userResponseModifiedAt = userResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= "UserResponses"
    , "userResponses" .= userResponses
    ]


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: "userResponses"
    pure $ UserResponses {
      userResponses = userResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON FunkyRecord where
  toJSON Boom1{..} = object $
    [ "tag" .= "FunkyRecord"
    , "boom1" .= boom1
    ]


instance FromJSON FunkyRecord where
  parseJSON (Object o) = do
    boom1 <- o .: "boom1"
    pure $ Boom1 {
      boom1 = boom1
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON FUnkyRecordP where
  toJSON FUnkyRecordP{..} = object $
    [ "tag" .= "FUnkyRecordP"
    , "funkyrecordpField" .= funkyrecordpField
    ]


instance FromJSON FUnkyRecordP where
  parseJSON (Object o) = do
    funkyrecordpField <- o .: "funkyrecordpField"
    pure $ FUnkyRecordP {
      funkyrecordpField = funkyrecordpField
    }
  parseJSON x = fail $ "Could not parse object: " <> show x

-- footer
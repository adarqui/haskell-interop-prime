{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop where




import Data.Aeson
import Data.Text   (Text)

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= "Session"
    , "unSession" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: "unSession"
    return $ Session {
      unSession = unSession
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
        return A

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
    return $ BigRecord {
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
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
    return $ User {
      userName = userName,
      userEmail = userEmail,
      userActive = userActive
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
    return $ UserRequest {
      userRequestName = userRequestName,
      userRequestEmail = userRequestEmail
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
    return $ UserResponse {
      userResponseId = userResponseId,
      userResponseName = userResponseName,
      userResponseEmail = userResponseEmail,
      userResponseActive = userResponseActive,
      userResponseCreatedAt = userResponseCreatedAt,
      userResponseModifiedAt = userResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance ToJSON FunkyRecord where
  toJSON Boom1{..} = object $
    [ "tag" .= "FunkyRecord"
    , "boom1" .= boom1
    ]


instance FromJSON FunkyRecord where
  parseJSON (Object o) = do
    boom1 <- o .: "boom1"
    return $ Boom1 {
      boom1 = boom1
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x

-- footer
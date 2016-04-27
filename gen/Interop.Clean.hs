{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop.Clean where




import Data.Aeson
import Data.Text   (Text)

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= "Session"
    , "un_session" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: "un_session"
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


instance ToJSON BigRecord where
  toJSON BigRecord{..} = object $
    [ "tag" .= "BigRecord"
    , "bool" .= bool
    , "int" .= int
    , "maybe_int" .= maybeInt
    , "integer" .= integer
    , "maybe_integer" .= maybeInteger
    , "string" .= string
    , "sum_type" .= sumType
    , "data'" .= data'
    , "class'" .= class'
    , "let'" .= let'
    , "module'" .= module'
    , "big_record" .= bigRecord
    ]


instance FromJSON BigRecord where
  parseJSON (Object o) = do
    bool <- o .: "bool"
    int <- o .: "int"
    maybeInt <- o .: "maybe_int"
    integer <- o .: "integer"
    maybeInteger <- o .: "maybe_integer"
    string <- o .: "string"
    sumType <- o .: "sum_type"
    data' <- o .: "data'"
    class' <- o .: "class'"
    let' <- o .: "let'"
    module' <- o .: "module'"
    bigRecord <- o .: "big_record"
    return $ BigRecord {
      bool = bool,
      int = int,
      maybeInt = maybeInt,
      integer = integer,
      maybeInteger = maybeInteger,
      string = string,
      sumType = sumType,
      data' = data',
      class' = class',
      let' = let',
      module' = module',
      bigRecord = bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance ToJSON User where
  toJSON User{..} = object $
    [ "tag" .= "User"
    , "name" .= name
    , "email" .= email
    ]


instance FromJSON User where
  parseJSON (Object o) = do
    name <- o .: "name"
    email <- o .: "email"
    return $ User {
      name = name,
      email = email
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance ToJSON UserRequest where
  toJSON UserRequest{..} = object $
    [ "tag" .= "UserRequest"
    , "name" .= name
    , "email" .= email
    ]


instance FromJSON UserRequest where
  parseJSON (Object o) = do
    name <- o .: "name"
    email <- o .: "email"
    return $ UserRequest {
      name = name,
      email = email
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= "UserResponse"
    , "id" .= id
    , "name" .= name
    , "email" .= email
    , "created_at" .= createdAt
    , "modified_at" .= modifiedAt
    ]


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    id <- o .: "id"
    name <- o .: "name"
    email <- o .: "email"
    createdAt <- o .: "created_at"
    modifiedAt <- o .: "modified_at"
    return $ UserResponse {
      id = id,
      name = name,
      email = email,
      createdAt = createdAt,
      modifiedAt = modifiedAt
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
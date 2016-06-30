{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}

module Interop.Clean where




import Data.Aeson
import Data.Text   (Text)
import Data.Monoid ((<>))

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= "Session"
    , "un_session" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: "un_session"
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
    , "bool" .= bool
    , "int" .= int
    , "maybe_int" .= maybeInt
    , "integer" .= integer
    , "maybe_integer" .= maybeInteger
    , "string" .= string
    , "string2" .= string2
    , "sum_type" .= sumType
    , "data" .= dataP
    , "class" .= classP
    , "let" .= letP
    , "module" .= moduleP
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
    string2 <- o .: "string2"
    sumType <- o .: "sum_type"
    dataP <- o .: "data"
    classP <- o .: "class"
    letP <- o .: "let"
    moduleP <- o .: "module"
    bigRecord <- o .: "big_record"
    pure $ BigRecord {
      bool = bool,
      int = int,
      maybeInt = maybeInt,
      integer = integer,
      maybeInteger = maybeInteger,
      string = string,
      string2 = string2,
      sumType = sumType,
      dataP = dataP,
      classP = classP,
      letP = letP,
      moduleP = moduleP,
      bigRecord = bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON User where
  toJSON User{..} = object $
    [ "tag" .= "User"
    , "name" .= name
    , "email" .= email
    , "active" .= active
    ]


instance FromJSON User where
  parseJSON (Object o) = do
    name <- o .: "name"
    email <- o .: "email"
    active <- o .: "active"
    pure $ User {
      name = name,
      email = email,
      active = active
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


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
    pure $ UserRequest {
      name = name,
      email = email
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= "UserResponse"
    , "id" .= id
    , "name" .= name
    , "email" .= email
    , "active" .= active
    , "created_at" .= createdAt
    , "modified_at" .= modifiedAt
    ]


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    id <- o .: "id"
    name <- o .: "name"
    email <- o .: "email"
    active <- o .: "active"
    createdAt <- o .: "created_at"
    modifiedAt <- o .: "modified_at"
    pure $ UserResponse {
      id = id,
      name = name,
      email = email,
      active = active,
      createdAt = createdAt,
      modifiedAt = modifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= "UserResponses"
    , "user_responses" .= userResponses
    ]


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: "user_responses"
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
    , "field" .= field
    ]


instance FromJSON FUnkyRecordP where
  parseJSON (Object o) = do
    field <- o .: "field"
    pure $ FUnkyRecordP {
      field = field
    }
  parseJSON x = fail $ "Could not parse object: " <> show x

-- footer
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Clean where




import Data.Aeson  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import Data.Text   (Text)
import Data.Monoid ((<>))

instance ToJSON Session where
  toJSON Session{..} = object $
    [ "tag" .= ("Session" :: Text)
    , "un_session" .= unSession
    ]


instance FromJSON Session where
  parseJSON (Object o) = do
    unSession <- o .: ("un_session" :: Text)
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
    bool <- o .: ("bool" :: Text)
    int <- o .: ("int" :: Text)
    maybeInt <- o .: ("maybe_int" :: Text)
    integer <- o .: ("integer" :: Text)
    maybeInteger <- o .: ("maybe_integer" :: Text)
    string <- o .: ("string" :: Text)
    string2 <- o .: ("string2" :: Text)
    sumType <- o .: ("sum_type" :: Text)
    dataP <- o .: ("data" :: Text)
    classP <- o .: ("class" :: Text)
    letP <- o .: ("let" :: Text)
    moduleP <- o .: ("module" :: Text)
    bigRecord <- o .: ("big_record" :: Text)
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


instance Show BigRecord where
    show rec = show "bool: " <> show (bool rec) <> ", " <> show "int: " <> show (int rec) <> ", " <> show "maybeInt: " <> show (maybeInt rec) <> ", " <> show "integer: " <> show (integer rec) <> ", " <> show "maybeInteger: " <> show (maybeInteger rec) <> ", " <> show "string: " <> show (string rec) <> ", " <> show "string2: " <> show (string2 rec) <> ", " <> show "sumType: " <> show (sumType rec) <> ", " <> show "dataP: " <> show (dataP rec) <> ", " <> show "classP: " <> show (classP rec) <> ", " <> show "letP: " <> show (letP rec) <> ", " <> show "moduleP: " <> show (moduleP rec) <> ", " <> show "bigRecord: " <> show (bigRecord rec)

instance Eq BigRecord where
  (==) a b = bool a == bool b && int a == int b && maybeInt a == maybeInt b && integer a == integer b && maybeInteger a == maybeInteger b && string a == string b && string2 a == string2 b && sumType a == sumType b && dataP a == dataP b && classP a == classP b && letP a == letP b && moduleP a == moduleP b && bigRecord a == bigRecord b

instance ToJSON User where
  toJSON User{..} = object $
    [ "tag" .= ("User" :: Text)
    , "name" .= name
    , "email" .= email
    , "active" .= active
    ]


instance FromJSON User where
  parseJSON (Object o) = do
    name <- o .: ("name" :: Text)
    email <- o .: ("email" :: Text)
    active <- o .: ("active" :: Text)
    pure $ User {
      name = name,
      email = email,
      active = active
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show User where
    show rec = show "name: " <> show (name rec) <> ", " <> show "email: " <> show (email rec) <> ", " <> show "active: " <> show (active rec)

instance Eq User where
  (==) a b = name a == name b && email a == email b && active a == active b

instance ToJSON UserRequest where
  toJSON UserRequest{..} = object $
    [ "tag" .= ("UserRequest" :: Text)
    , "name" .= name
    , "email" .= email
    ]


instance FromJSON UserRequest where
  parseJSON (Object o) = do
    name <- o .: ("name" :: Text)
    email <- o .: ("email" :: Text)
    pure $ UserRequest {
      name = name,
      email = email
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show UserRequest where
    show rec = show "name: " <> show (name rec) <> ", " <> show "email: " <> show (email rec)

instance Eq UserRequest where
  (==) a b = name a == name b && email a == email b

instance ToJSON UserResponse where
  toJSON UserResponse{..} = object $
    [ "tag" .= ("UserResponse" :: Text)
    , "id" .= id
    , "name" .= name
    , "email" .= email
    , "active" .= active
    , "created_at" .= createdAt
    , "modified_at" .= modifiedAt
    ]


instance FromJSON UserResponse where
  parseJSON (Object o) = do
    id <- o .: ("id" :: Text)
    name <- o .: ("name" :: Text)
    email <- o .: ("email" :: Text)
    active <- o .: ("active" :: Text)
    createdAt <- o .: ("created_at" :: Text)
    modifiedAt <- o .: ("modified_at" :: Text)
    pure $ UserResponse {
      id = id,
      name = name,
      email = email,
      active = active,
      createdAt = createdAt,
      modifiedAt = modifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show UserResponse where
    show rec = show "id: " <> show (id rec) <> ", " <> show "name: " <> show (name rec) <> ", " <> show "email: " <> show (email rec) <> ", " <> show "active: " <> show (active rec) <> ", " <> show "createdAt: " <> show (createdAt rec) <> ", " <> show "modifiedAt: " <> show (modifiedAt rec)

instance Eq UserResponse where
  (==) a b = id a == id b && name a == name b && email a == email b && active a == active b && createdAt a == createdAt b && modifiedAt a == modifiedAt b

instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= ("UserResponses" :: Text)
    , "user_responses" .= userResponses
    ]


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: ("user_responses" :: Text)
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
    , "field" .= field
    ]


instance FromJSON FUnkyRecordP where
  parseJSON (Object o) = do
    field <- o .: ("field" :: Text)
    pure $ FUnkyRecordP {
      field = field
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance Show FUnkyRecordP where
    show rec = show "field: " <> show (field rec)

instance Eq FUnkyRecordP where
  (==) a b = field a == field b
-- footer
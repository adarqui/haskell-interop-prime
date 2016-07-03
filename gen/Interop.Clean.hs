{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Interop.Clean where




import           Data.Aeson          (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Monoid         ((<>))
import           Haskell.Api.Helpers (QueryParam, qp)

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
  show A = "a"
  show (B x0) = "b: " <> show x0
  show (C x0) = "c: " <> show x0
  show (D x0) = "d: " <> show x0
  show (E x0) = "e: " <> show x0
  show (F x0) = "f: " <> show x0
  show (G x0) = "g: " <> show x0
  show (H x0 x1 x2 x3) = "h: " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3


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
  show (Limit x0) = "limit: " <> show x0
  show (Offset x0) = "offset: " <> show x0
  show (ByUsersIds x0) = "by_users_ids: " <> show x0
  show (ByUserNameText x0) = "by_user_name_text: " <> show x0
  show (ByUserNameStr x0) = "by_user_name_str: " <> show x0
  show (ByUsersNames x0) = "by_users_names: " <> show x0
  show (ByUsersEmails x0) = "by_users_emails: " <> show x0
  show (ByUserActive x0) = "by_user_active: " <> show x0


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
  qp (Limit x0) = ("limit", (T.pack $ show x0))
  qp (Offset x0) = ("offset", (T.pack $ show x0))
  qp (ByUsersIds x0) = ("by_users_ids", (T.pack $ show x0))
  qp (ByUserNameText x0) = ("by_user_name_text", x0)
  qp (ByUserNameStr x0) = ("by_user_name_str", (T.pack x0))
  qp (ByUsersNames x0) = ("by_users_names", (T.pack $ show x0))
  qp (ByUsersEmails x0) = ("by_users_emails", (T.pack $ show x0))
  qp (ByUserActive x0) = ("by_user_active", (T.pack $ show x0))


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
  show ParamTag_Limit = "limit"
  show ParamTag_Offset = "offset"
  show ParamTag_ByUsersIds = "by_users_ids"
  show ParamTag_ByUsersNames = "by_users_names"
  show ParamTag_ByUsersEmails = "by_users_emails"
  show ParamTag_ByUserActive = "by_user_active"


instance Read ParamTag where
  readsPrec _ "limit" = [(ParamTag_Limit, "")]
  readsPrec _ "offset" = [(ParamTag_Offset, "")]
  readsPrec _ "by_users_ids" = [(ParamTag_ByUsersIds, "")]
  readsPrec _ "by_users_names" = [(ParamTag_ByUsersNames, "")]
  readsPrec _ "by_users_emails" = [(ParamTag_ByUsersEmails, "")]
  readsPrec _ "by_user_active" = [(ParamTag_ByUserActive, "")]
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
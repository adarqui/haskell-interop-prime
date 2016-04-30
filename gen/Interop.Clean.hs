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
    , "bool" .= bool
    , "int" .= int
    , "maybe_int" .= maybeInt
    , "integer" .= integer
    , "maybe_integer" .= maybeInteger
    , "string" .= string
    , "string2" .= string2
    , "sum_type" .= sumType
    , "data_p" .= dataP
    , "class_p" .= classP
    , "let_p" .= letP
    , "module_p" .= moduleP
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
    dataP <- o .: "data_p"
    classP <- o .: "class_p"
    letP <- o .: "let_p"
    moduleP <- o .: "module_p"
    bigRecord <- o .: "big_record"
    return $ BigRecord {
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
  parseJSON x = fail $ "Could not parse object: " ++ show x


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
    return $ User {
      name = name,
      email = email,
      active = active
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


instance ToJSON UserResponses where
  toJSON UserResponses{..} = object $
    [ "tag" .= "UserResponses"
    , "user_responses" .= userResponses
    ]


instance FromJSON UserResponses where
  parseJSON (Object o) = do
    userResponses <- o .: "user_responses"
    return $ UserResponses {
      userResponses = userResponses
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
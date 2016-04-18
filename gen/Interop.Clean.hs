module Interop.Clean where


import Data.Aeson

instance ToJSON Session where
  toJSON (Session v) = object $
    [ "tag" .= "Session"
    , "un_session" .= v.unSession
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
  toJSON (BigRecord v) = object $
    [ "tag" .= "BigRecord"
    , "bool" .= v.bool
    , "int" .= v.int
    , "maybe_int" .= v.maybeInt
    , "integer" .= v.integer
    , "maybe_integer" .= v.maybeInteger
    , "string" .= v.string
    , "sum_type" .= v.sumType
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
    return $ BigRecord {
      bool = bool,
      int = int,
      maybeInt = maybeInt,
      integer = integer,
      maybeInteger = maybeInteger,
      string = string,
      sumType = sumType
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance ToJSON FunkyRecord where
  toJSON (Boom1 v) = object $
    [ "tag" .= "FunkyRecord"
    , "boom1" .= v.boom1
    ]


instance FromJSON FunkyRecord where
  parseJSON (Object o) = do
    boom1 <- o .: "boom1"
    return $ Boom1 {
      boom1 = boom1
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x

-- footer
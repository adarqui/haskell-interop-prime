module Interop.Clean where


import Control.Monad.Aff
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Printer
import Data.Either
import Data.Foreign (readString)
import Data.Foreign.Class
import Data.JSON
import Data.List (List ())
import Data.Maybe
import Data.Set (Set ())
import Data.Tuple
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Optic.Lens
import Optic.Core
import Prelude

newtype Session = Session {
  unSession :: String
}


_Session :: LensP Session {
  unSession :: String
}
_Session f (Session o) = Session <$> f o


mkSession :: String -> Session
mkSession unSession =
  Session{unSession}


unwrapSession (Session r) = r

instance sessionToJson :: ToJSON Session where
  toJSON (Session v) = object $
    [ "tag" .= "Session"
    , "un_session" .= v.unSession
    ]


instance sessionFromJSON :: FromJSON Session where
  parseJSON (JObject o) = do
    unSession <- o .: "un_session"
    return $ Session {
      unSession : unSession
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance sessionEncodeJson :: EncodeJson Session where
  encodeJson (Session o) =
       "tag" := "Session"
    ~> "un_session" := o.unSession
    ~> jsonEmptyObject


instance sessionDecodeJson :: DecodeJson Session where
  decodeJson o = do
    obj <- decodeJson o
    unSession <- obj .? "un_session"
    pure $ Session {
      unSession
    }


instance sessionRequestable :: Requestable Session where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sessionRespondable :: Respondable Session where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


instance sessionIsForeign :: IsForeign Session where
  read f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


data SumType
  = A 
  | B Int
  | C Boolean
  | D String
  | E (Array  Int)
  | F SumType
  | G (Array  SumType)
  | H Boolean Int String (Maybe Boolean)



instance sumTypeToJSON :: ToJSON SumType where
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


instance sumTypeFromJSON :: FromJSON SumType where
  parseJSON (JObject o) = do
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


instance sumTypeEncodeJson :: EncodeJson SumType where
  encodeJson (A ) =
       "tag" := "A"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (B x0) =
       "tag" := "B"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (C x0) =
       "tag" := "C"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (D x0) =
       "tag" := "D"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (E x0) =
       "tag" := "E"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (F x0) =
       "tag" := "F"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (G x0) =
       "tag" := "G"
    ~> "contents" := encodeJson x0
    ~> jsonEmptyObject
  encodeJson (H x0 x1 x2 x3) =
       "tag" := "H"
    ~> "contents" := [encodeJson x0, encodeJson x1, encodeJson x2, encodeJson x3]
    ~> jsonEmptyObject


instance sumTypeDecodeJson :: DecodeJson SumType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
        "A" -> do
          return $ A

        "B" -> do
          x0 <- obj .? "contents"
          B <$> decodeJson x0

        "C" -> do
          x0 <- obj .? "contents"
          C <$> decodeJson x0

        "D" -> do
          x0 <- obj .? "contents"
          D <$> decodeJson x0

        "E" -> do
          x0 <- obj .? "contents"
          E <$> decodeJson x0

        "F" -> do
          x0 <- obj .? "contents"
          F <$> decodeJson x0

        "G" -> do
          x0 <- obj .? "contents"
          G <$> decodeJson x0

        "H" -> do
          [x0, x1, x2, x3] <- obj .? "contents"
          H <$> decodeJson x0 <*> decodeJson x1 <*> decodeJson x2 <*> decodeJson x3

  decodeJson x = fail $ "Could not parse object: " ++ show x


instance sumTypeRequestable :: Requestable SumType where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sumTypeRespondable :: Respondable SumType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


instance sumTypeIsForeign :: IsForeign SumType where
  read f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


instance sumTypeShow :: Show SumType where
  show (A) = "A"
  show (B x0) = "B: " ++ show x0
  show (C x0) = "C: " ++ show x0
  show (D x0) = "D: " ++ show x0
  show (E x0) = "E: " ++ show x0
  show (F x0) = "F: " ++ show x0
  show (G x0) = "G: " ++ show x0
  show (H x0 x1 x2 x3) = "H: " ++ show x0 ++ " " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3


newtype BigRecord = BigRecord {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  sumType :: SumType
}


_BigRecord :: LensP BigRecord {
  bool :: Boolean,
  int :: Int,
  maybeInt :: (Maybe Int),
  integer :: Int,
  maybeInteger :: (Maybe Int),
  string :: String,
  sumType :: SumType
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> SumType -> BigRecord
mkBigRecord bool int maybeInt integer maybeInteger string sumType =
  BigRecord{bool, int, maybeInt, integer, maybeInteger, string, sumType}


unwrapBigRecord (BigRecord r) = r

instance bigRecordToJson :: ToJSON BigRecord where
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


instance bigRecordFromJSON :: FromJSON BigRecord where
  parseJSON (JObject o) = do
    bool <- o .: "bool"
    int <- o .: "int"
    maybeInt <- o .: "maybe_int"
    integer <- o .: "integer"
    maybeInteger <- o .: "maybe_integer"
    string <- o .: "string"
    sumType <- o .: "sum_type"
    return $ BigRecord {
      bool : bool,
      int : int,
      maybeInt : maybeInt,
      integer : integer,
      maybeInteger : maybeInteger,
      string : string,
      sumType : sumType
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance bigRecordEncodeJson :: EncodeJson BigRecord where
  encodeJson (BigRecord o) =
       "tag" := "BigRecord"
    ~> "bool" := o.bool
    ~> "int" := o.int
    ~> "maybe_int" := o.maybeInt
    ~> "integer" := o.integer
    ~> "maybe_integer" := o.maybeInteger
    ~> "string" := o.string
    ~> "sum_type" := o.sumType
    ~> jsonEmptyObject


instance bigRecordDecodeJson :: DecodeJson BigRecord where
  decodeJson o = do
    obj <- decodeJson o
    bool <- obj .? "bool"
    int <- obj .? "int"
    maybeInt <- obj .? "maybe_int"
    integer <- obj .? "integer"
    maybeInteger <- obj .? "maybe_integer"
    string <- obj .? "string"
    sumType <- obj .? "sum_type"
    pure $ BigRecord {
      bool,
      int,
      maybeInt,
      integer,
      maybeInteger,
      string,
      sumType
    }


instance bigRecordRequestable :: Requestable BigRecord where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance bigRecordRespondable :: Respondable BigRecord where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


instance bigRecordIsForeign :: IsForeign BigRecord where
  read f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


type Text = String


type TextMaybe = (Maybe String)


newtype FunkyRecord = Boom1 {
  boom1 :: Boolean
}


_FunkyRecord :: LensP FunkyRecord {
  boom1 :: Boolean
}
_FunkyRecord f (Boom1 o) = Boom1 <$> f o


mkFunkyRecord :: Boolean -> FunkyRecord
mkFunkyRecord boom1 =
  Boom1{boom1}


unwrapFunkyRecord (Boom1 r) = r

instance funkyRecordToJson :: ToJSON FunkyRecord where
  toJSON (Boom1 v) = object $
    [ "tag" .= "FunkyRecord"
    , "boom1" .= v.boom1
    ]


instance funkyRecordFromJSON :: FromJSON FunkyRecord where
  parseJSON (JObject o) = do
    boom1 <- o .: "boom1"
    return $ Boom1 {
      boom1 : boom1
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance funkyRecordEncodeJson :: EncodeJson FunkyRecord where
  encodeJson (Boom1 o) =
       "tag" := "FunkyRecord"
    ~> "boom1" := o.boom1
    ~> jsonEmptyObject


instance funkyRecordDecodeJson :: DecodeJson FunkyRecord where
  decodeJson o = do
    obj <- decodeJson o
    boom1 <- obj .? "boom1"
    pure $ Boom1 {
      boom1
    }


instance funkyRecordRequestable :: Requestable FunkyRecord where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance funkyRecordRespondable :: Respondable FunkyRecord where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


instance funkyRecordIsForeign :: IsForeign FunkyRecord where
  read f = case readString f of
    Right s -> readJSON s
    Left er -> Left er


bool_ :: forall b a r. Lens { bool :: a | r } { bool :: b | r } a b
bool_ f o = o { bool = _ } <$> f o.bool


int_ :: forall b a r. Lens { int :: a | r } { int :: b | r } a b
int_ f o = o { int = _ } <$> f o.int


maybeInt_ :: forall b a r. Lens { maybeInt :: a | r } { maybeInt :: b | r } a b
maybeInt_ f o = o { maybeInt = _ } <$> f o.maybeInt


integer_ :: forall b a r. Lens { integer :: a | r } { integer :: b | r } a b
integer_ f o = o { integer = _ } <$> f o.integer


maybeInteger_ :: forall b a r. Lens { maybeInteger :: a | r } { maybeInteger :: b | r } a b
maybeInteger_ f o = o { maybeInteger = _ } <$> f o.maybeInteger


string_ :: forall b a r. Lens { string :: a | r } { string :: b | r } a b
string_ f o = o { string = _ } <$> f o.string


sumType_ :: forall b a r. Lens { sumType :: a | r } { sumType :: b | r } a b
sumType_ f o = o { sumType = _ } <$> f o.sumType


boom1_ :: forall b a r. Lens { boom1 :: a | r } { boom1 :: b | r } a b
boom1_ f o = o { boom1 = _ } <$> f o.boom1


unSession_ :: forall b a r. Lens { unSession :: a | r } { unSession :: b | r } a b
unSession_ f o = o { unSession = _ } <$> f o.unSession

-- footer
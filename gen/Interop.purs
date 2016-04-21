module Interop where


import Control.Monad.Aff
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Printer
import Data.Date
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
    , "unSession" .= v.unSession
    ]


instance sessionFromJSON :: FromJSON Session where
  parseJSON (JObject o) = do
    unSession <- o .: "unSession"
    return $ Session {
      unSession : unSession
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance sessionEncodeJson :: EncodeJson Session where
  encodeJson (Session o) =
       "tag" := "Session"
    ~> "unSession" := o.unSession
    ~> jsonEmptyObject


instance sessionDecodeJson :: DecodeJson Session where
  decodeJson o = do
    obj <- decodeJson o
    unSession <- obj .? "unSession"
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
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}


_BigRecord :: LensP BigRecord {
  bigRecordBool :: Boolean,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: (Maybe Int),
  bigRecordInteger :: Int,
  bigRecordMaybeInteger :: (Maybe Int),
  bigRecordString :: String,
  bigRecordSumType :: SumType,
  bigRecordData :: String,
  bigRecordClass :: String,
  bigRecordLet :: String,
  bigRecordModule :: String,
  bigRecord :: Boolean
}
_BigRecord f (BigRecord o) = BigRecord <$> f o


mkBigRecord :: Boolean -> Int -> (Maybe Int) -> Int -> (Maybe Int) -> String -> SumType -> String -> String -> String -> String -> Boolean -> BigRecord
mkBigRecord bigRecordBool bigRecordInt bigRecordMaybeInt bigRecordInteger bigRecordMaybeInteger bigRecordString bigRecordSumType bigRecordData bigRecordClass bigRecordLet bigRecordModule bigRecord =
  BigRecord{bigRecordBool, bigRecordInt, bigRecordMaybeInt, bigRecordInteger, bigRecordMaybeInteger, bigRecordString, bigRecordSumType, bigRecordData, bigRecordClass, bigRecordLet, bigRecordModule, bigRecord}


unwrapBigRecord (BigRecord r) = r

instance bigRecordToJson :: ToJSON BigRecord where
  toJSON (BigRecord v) = object $
    [ "tag" .= "BigRecord"
    , "bigRecordBool" .= v.bigRecordBool
    , "bigRecordInt" .= v.bigRecordInt
    , "bigRecordMaybeInt" .= v.bigRecordMaybeInt
    , "bigRecordInteger" .= v.bigRecordInteger
    , "bigRecordMaybeInteger" .= v.bigRecordMaybeInteger
    , "bigRecordString" .= v.bigRecordString
    , "bigRecordSumType" .= v.bigRecordSumType
    , "bigRecordData" .= v.bigRecordData
    , "bigRecordClass" .= v.bigRecordClass
    , "bigRecordLet" .= v.bigRecordLet
    , "bigRecordModule" .= v.bigRecordModule
    , "bigRecord" .= v.bigRecord
    ]


instance bigRecordFromJSON :: FromJSON BigRecord where
  parseJSON (JObject o) = do
    bigRecordBool <- o .: "bigRecordBool"
    bigRecordInt <- o .: "bigRecordInt"
    bigRecordMaybeInt <- o .: "bigRecordMaybeInt"
    bigRecordInteger <- o .: "bigRecordInteger"
    bigRecordMaybeInteger <- o .: "bigRecordMaybeInteger"
    bigRecordString <- o .: "bigRecordString"
    bigRecordSumType <- o .: "bigRecordSumType"
    bigRecordData <- o .: "bigRecordData"
    bigRecordClass <- o .: "bigRecordClass"
    bigRecordLet <- o .: "bigRecordLet"
    bigRecordModule <- o .: "bigRecordModule"
    bigRecord <- o .: "bigRecord"
    return $ BigRecord {
      bigRecordBool : bigRecordBool,
      bigRecordInt : bigRecordInt,
      bigRecordMaybeInt : bigRecordMaybeInt,
      bigRecordInteger : bigRecordInteger,
      bigRecordMaybeInteger : bigRecordMaybeInteger,
      bigRecordString : bigRecordString,
      bigRecordSumType : bigRecordSumType,
      bigRecordData : bigRecordData,
      bigRecordClass : bigRecordClass,
      bigRecordLet : bigRecordLet,
      bigRecordModule : bigRecordModule,
      bigRecord : bigRecord
    }
  parseJSON x = fail $ "Could not parse object: " ++ show x


instance bigRecordEncodeJson :: EncodeJson BigRecord where
  encodeJson (BigRecord o) =
       "tag" := "BigRecord"
    ~> "bigRecordBool" := o.bigRecordBool
    ~> "bigRecordInt" := o.bigRecordInt
    ~> "bigRecordMaybeInt" := o.bigRecordMaybeInt
    ~> "bigRecordInteger" := o.bigRecordInteger
    ~> "bigRecordMaybeInteger" := o.bigRecordMaybeInteger
    ~> "bigRecordString" := o.bigRecordString
    ~> "bigRecordSumType" := o.bigRecordSumType
    ~> "bigRecordData" := o.bigRecordData
    ~> "bigRecordClass" := o.bigRecordClass
    ~> "bigRecordLet" := o.bigRecordLet
    ~> "bigRecordModule" := o.bigRecordModule
    ~> "bigRecord" := o.bigRecord
    ~> jsonEmptyObject


instance bigRecordDecodeJson :: DecodeJson BigRecord where
  decodeJson o = do
    obj <- decodeJson o
    bigRecordBool <- obj .? "bigRecordBool"
    bigRecordInt <- obj .? "bigRecordInt"
    bigRecordMaybeInt <- obj .? "bigRecordMaybeInt"
    bigRecordInteger <- obj .? "bigRecordInteger"
    bigRecordMaybeInteger <- obj .? "bigRecordMaybeInteger"
    bigRecordString <- obj .? "bigRecordString"
    bigRecordSumType <- obj .? "bigRecordSumType"
    bigRecordData <- obj .? "bigRecordData"
    bigRecordClass <- obj .? "bigRecordClass"
    bigRecordLet <- obj .? "bigRecordLet"
    bigRecordModule <- obj .? "bigRecordModule"
    bigRecord <- obj .? "bigRecord"
    pure $ BigRecord {
      bigRecordBool,
      bigRecordInt,
      bigRecordMaybeInt,
      bigRecordInteger,
      bigRecordMaybeInteger,
      bigRecordString,
      bigRecordSumType,
      bigRecordData,
      bigRecordClass,
      bigRecordLet,
      bigRecordModule,
      bigRecord
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


bigRecordBool_ :: forall b a r. Lens { bigRecordBool :: a | r } { bigRecordBool :: b | r } a b
bigRecordBool_ f o = o { bigRecordBool = _ } <$> f o.bigRecordBool


bigRecordInt_ :: forall b a r. Lens { bigRecordInt :: a | r } { bigRecordInt :: b | r } a b
bigRecordInt_ f o = o { bigRecordInt = _ } <$> f o.bigRecordInt


bigRecordMaybeInt_ :: forall b a r. Lens { bigRecordMaybeInt :: a | r } { bigRecordMaybeInt :: b | r } a b
bigRecordMaybeInt_ f o = o { bigRecordMaybeInt = _ } <$> f o.bigRecordMaybeInt


bigRecordInteger_ :: forall b a r. Lens { bigRecordInteger :: a | r } { bigRecordInteger :: b | r } a b
bigRecordInteger_ f o = o { bigRecordInteger = _ } <$> f o.bigRecordInteger


bigRecordMaybeInteger_ :: forall b a r. Lens { bigRecordMaybeInteger :: a | r } { bigRecordMaybeInteger :: b | r } a b
bigRecordMaybeInteger_ f o = o { bigRecordMaybeInteger = _ } <$> f o.bigRecordMaybeInteger


bigRecordString_ :: forall b a r. Lens { bigRecordString :: a | r } { bigRecordString :: b | r } a b
bigRecordString_ f o = o { bigRecordString = _ } <$> f o.bigRecordString


bigRecordSumType_ :: forall b a r. Lens { bigRecordSumType :: a | r } { bigRecordSumType :: b | r } a b
bigRecordSumType_ f o = o { bigRecordSumType = _ } <$> f o.bigRecordSumType


bigRecordData_ :: forall b a r. Lens { bigRecordData :: a | r } { bigRecordData :: b | r } a b
bigRecordData_ f o = o { bigRecordData = _ } <$> f o.bigRecordData


bigRecordClass_ :: forall b a r. Lens { bigRecordClass :: a | r } { bigRecordClass :: b | r } a b
bigRecordClass_ f o = o { bigRecordClass = _ } <$> f o.bigRecordClass


bigRecordLet_ :: forall b a r. Lens { bigRecordLet :: a | r } { bigRecordLet :: b | r } a b
bigRecordLet_ f o = o { bigRecordLet = _ } <$> f o.bigRecordLet


bigRecordModule_ :: forall b a r. Lens { bigRecordModule :: a | r } { bigRecordModule :: b | r } a b
bigRecordModule_ f o = o { bigRecordModule = _ } <$> f o.bigRecordModule


bigRecord_ :: forall b a r. Lens { bigRecord :: a | r } { bigRecord :: b | r } a b
bigRecord_ f o = o { bigRecord = _ } <$> f o.bigRecord


boom1_ :: forall b a r. Lens { boom1 :: a | r } { boom1 :: b | r } a b
boom1_ f o = o { boom1 = _ } <$> f o.boom1


unSession_ :: forall b a r. Lens { unSession :: a | r } { unSession :: b | r } a b
unSession_ f o = o { unSession = _ } <$> f o.unSession

-- footer
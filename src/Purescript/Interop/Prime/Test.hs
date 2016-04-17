{-# LANGUAGE TemplateHaskell #-}

module Purescript.Interop.Prime.Test where



import           Purescript.Interop.Prime



newtype Session = Session { unSession :: String }

data SumType
  = A
  | B Int
  | C Bool
  | D String
  | E [Int]
  | F SumType
  | G [SumType]
  | H Bool Int String (Maybe Bool)

data BigRecord = BigRecord {
  bigRecordBool         :: Bool,
  bigRecordInt          :: Int,
  bigRecordMaybeInt     :: Maybe Int,
  bigRecordInteger      :: Integer,
  bigRecordMaybeInteger :: Maybe Integer,
  bigRecordString       :: String,
  bigRecordSumType      :: SumType
}

newtype DateMaybe = DateMaybe (Maybe String)

type Text = String

type TextMaybe = Maybe Text

data FunkyRecord
  = Boom1 { boom1 :: Bool }
  | Boom2 { boom2 :: Bool }
  | Boom3 { boom3a :: Bool, boom3b :: Bool, boom3c :: Bool }
  | Boom4



mkExports defaultOptionsPurescript (Just ("module Interop where", "-- footer", "/tmp/Interop.purs")) defaultPurescriptMkGs
  [
    (''Session, defaultPurescriptMks),
    (''SumType, defaultPurescriptMks),
    (''BigRecord, defaultPurescriptMks),
    (''DateMaybe, defaultPurescriptMks),
    (''Text, defaultPurescriptMks),
    (''TextMaybe, defaultPurescriptMks),
    (''FunkyRecord, defaultPurescriptMks)
  ]



mkExports defaultOptionsCleanPurescript (Just ("module Interop.Clean where", "-- footer", "/tmp/Interop.Clean.purs")) defaultPurescriptMkGs
  [
    (''Session, defaultPurescriptMks),
    (''SumType, defaultPurescriptMks),
    (''BigRecord, defaultPurescriptMks),
    (''DateMaybe, defaultPurescriptMks),
    (''Text, defaultPurescriptMks),
    (''TextMaybe, defaultPurescriptMks),
    (''FunkyRecord, defaultPurescriptMks)
  ]



mkExports defaultOptionsCleanHaskell (Just ("module Interop where", "-- footer", "/tmp/Interop.hs")) defaultHaskellMkGs
  [
    (''Session, defaultHaskellMks),
    (''SumType, defaultHaskellMks),
    (''BigRecord, defaultHaskellMks),
    (''DateMaybe, defaultHaskellMks),
    (''Text, defaultHaskellMks),
    (''TextMaybe, defaultHaskellMks),
    (''FunkyRecord, defaultHaskellMks)
  ]

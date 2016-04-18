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



mkExports
  (Options
    (defaultOptionsPurescript "/tmp/Interop.purs")
    (defaultPurescriptMkGs "module Interop where")
    (defaultOptionsHaskell "/tmp/Interop.hs")
    (defaultHaskellMkGs "module Interop where"))
  [
    (''Session,     defaultPurescriptMks, defaultHaskellMks),
    (''SumType,     defaultPurescriptMks, defaultHaskellMks),
    (''BigRecord,   defaultPurescriptMks, defaultHaskellMks),
    (''DateMaybe,   defaultPurescriptMks, defaultHaskellMks),
    (''Text,        defaultPurescriptMks, defaultHaskellMks),
    (''TextMaybe,   defaultPurescriptMks, defaultHaskellMks),
    (''FunkyRecord, defaultPurescriptMks, defaultHaskellMks)
  ]



mkExports
  (Options
    (defaultOptionsCleanPurescript "/tmp/Interop.Clean.purs")
    (defaultPurescriptMkGs "module Interop.Clean where")
    (defaultOptionsCleanHaskell "/tmp/Interop.Clean.hs")
    (defaultHaskellMkGs "module Interop.Clean where"))
  [
    (''Session,     defaultPurescriptMks, defaultHaskellMks),
    (''SumType,     defaultPurescriptMks, defaultHaskellMks),
    (''BigRecord,   defaultPurescriptMks, defaultHaskellMks),
    (''DateMaybe,   defaultPurescriptMks, defaultHaskellMks),
    (''Text,        defaultPurescriptMks, defaultHaskellMks),
    (''TextMaybe,   defaultPurescriptMks, defaultHaskellMks),
    (''FunkyRecord, defaultPurescriptMks, defaultHaskellMks)
  ]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS -ddump-splices      #-}

module Haskell.Interop.Prime.Types (
  ExportT,
  Lang (..),
  MkTypeOpts (..),
  isMkTypeOpt_Deriving,
  isMkTypeOpts_StrictFields,
  Deriving (..),
  mkTypeOpts_DerivingToString,
  derivingToString,
  Mk (..),
  MkG (..),
  Options (..),
  InteropOptions (..),
  InternalRep (..),
  InteropReader (..),
  InteropState (..),
  Api (..),
  ApiMethod (..),
  ApiParam (..),
  ApiEntry (..),
  Api_TH (..),
  ApiMethod_TH (..),
  ApiParam_TH (..),
  ApiEntry_TH (..),
  StringTransformFn,
) where



import           Control.Monad.Trans.RWS (RWS)
import qualified Data.Map                as M
import           Language.Haskell.TH



data Lang
  = LangPurescript
  | LangHaskell
  deriving (Show, Eq, Ord)



data MkTypeOpts
  = MkTypeOpts_StrictFields
  | MkTypeOpts_Deriving Deriving
  deriving (Show, Eq, Ord)

isMkTypeOpt_Deriving :: MkTypeOpts -> Bool
isMkTypeOpt_Deriving (MkTypeOpts_Deriving _) = True
isMkTypeOpt_Deriving _                       = False

isMkTypeOpts_StrictFields :: [MkTypeOpts] -> Bool
isMkTypeOpts_StrictFields = any (==MkTypeOpts_StrictFields)



data Deriving
  = Deriving_Generic
  | Deriving_Typeable
  | Deriving_NFData
  | Deriving_Show
  | Deriving_Read
  | Deriving_Eq
  | Deriving_Ord
  | Deriving_Enum
  deriving (Show, Eq, Ord)

derivingToString :: Deriving -> String
derivingToString der =
  case der of
    Deriving_Generic  -> "Generic"
    Deriving_Typeable -> "Typeable"
    Deriving_NFData   -> "NFData"
    Deriving_Show     -> "Show"
    Deriving_Read     -> "Read"
    Deriving_Eq       -> "Eq"
    Deriving_Ord      -> "Ord"
    Deriving_Enum     -> "Enum"

mkTypeOpts_DerivingToString :: MkTypeOpts -> Maybe String
mkTypeOpts_DerivingToString (MkTypeOpts_Deriving der) = Just $ derivingToString der
mkTypeOpts_DerivingToString _                         = Nothing



data Mk
  = MkType
  | MkTypeRows String
  | MkTypeWith [MkTypeOpts]
  | MkToJSON
  | MkFromJSON
  | MkUnwrap
  | MkMk
  | MkLens
  | MkEncodeJson
  | MkDecodeJson
  | MkRequestable
  | MkRespondable
  | MkIsForeign
  | MkShow
  | MkRead
  | MkEq
  | MkDefault String
  -- haskell-api-helpers & purescript-api-helpers
  | MkQueryParam
  deriving (Show, Eq, Ord)



data MkG
  = MkGPurescriptImports
  | MkGPurescriptApiImports
  | MkGPurescriptApiStringImports
  | MkGPurescriptConvertImports
  | MkGHaskellImports
  | MkGHaskellApiImports
  | MkGHaskellApiStringImports
  | MkGHaskellConvertImports
  | MkGLensFields
  | MkGHeader String
  | MkGFooter String
  deriving (Show, Eq, Ord)



type StringTransformFn = String -> String -> String



data InteropOptions = InteropOptions {
  fieldNameTransform   :: StringTransformFn,
  jsonNameTransform    :: StringTransformFn,
  jsonTagNameTransform :: StringTransformFn,
  spacingNL            :: Int,
  spacingIndent        :: Int,
  typeMap              :: M.Map String String,
  reservedMap          :: M.Map String String,
  lang                 :: Lang,
  psDataToNewtype      :: Bool,
  filePath             :: FilePath,
  debug                :: Bool
}



data Options = Options {
  psInterop :: InteropOptions,
  psMkGs    :: [MkG],
  hsInterop :: InteropOptions,
  hsMkGs    :: [MkG]
}



data InternalRep
  = NewtypeRecIR String String [(String, String)]
  | NewtypeNormalIR String String
  | DataRecIR String String [(String, String)]
  | DataNormalIR String [(String, [String])]
  | TypeIR String [String] String
  | EmptyIR
  deriving (Show)



data InteropReader = InteropReader {
  irInterop :: InteropOptions,
  irFields  :: [String]
}


data InteropState = InteropState {
  isRep :: InternalRep
}



data Api = Api {
  apiPrefix  :: String,
  apiEntries :: [ApiEntry]
} deriving (Show)



data ApiMethod
  = ApiGET    String
  | ApiPOST   String String
  | ApiPUT    String String
  | ApiDELETE String
  deriving (Show)



data ApiParam
  = Par [(String, String)]
  | ParBy String String
  | ParBoth [(String, String)] (String, String)
  | ParNone
  deriving (Show)



data ApiEntry
  = ApiEntry String [ApiParam] [ApiMethod]
  deriving (Show)



data Api_TH = Api_TH {
  apiPrefix_TH  :: String,
  apiEntries_TH :: [ApiEntry_TH]
} deriving (Show)



data ApiMethod_TH
  = ApiGET_TH    Name
  | ApiPOST_TH   Name Name
  | ApiPUT_TH    Name Name
  | ApiDELETE_TH Name
  deriving (Show)



data ApiParam_TH
  = Par_TH [(String, Name)]
  | ParBy_TH String Name
  | ParBoth_TH [(String, Name)] (String, Name)
  | ParNone_TH
  deriving (Show)



data ApiEntry_TH
  = ApiEntry_TH String [ApiParam_TH] [ApiMethod_TH]
  deriving (Show)



-- newtype RWS r w s a
type ExportT = RWS InteropReader () InteropState

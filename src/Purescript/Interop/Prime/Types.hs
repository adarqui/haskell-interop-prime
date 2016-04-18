{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS -ddump-splices      #-}

module Purescript.Interop.Prime.Types (
  ExportT,
  Lang (..),
  Mk (..),
  MkG (..),
  Options (..),
  InteropOptions (..),
  InternalRep (..),
  InteropReader (..),
  InteropState (..),
  StringTransformFn,
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as M



data Lang
  = LangPurescript
  | LangHaskell
  deriving (Show)



data Mk
  = MkType
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
  deriving (Show)



data MkG
  = MkGPurescriptImports
  | MkGHaskellImports
  | MkGLensFields
  | MkGHeader String
  | MkGFooter String
  deriving (Show)



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
  filePath             :: FilePath
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
  | TypeIR String String
  | EmptyIR
  deriving (Show)



data InteropReader = InteropReader {
  irInterop :: InteropOptions,
  irFields  :: [String]
}


data InteropState = InteropState {
  isRep :: InternalRep
}


-- newtype RWST r w s m a
type ExportT = RWS InteropReader () InteropState

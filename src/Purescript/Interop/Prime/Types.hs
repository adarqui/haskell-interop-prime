{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -ddump-splices #-}

module Purescript.Interop.Prime.Types (
  Lang (..),
  Mk (..),
  MkG (..),
  InteropOptions (..),
  InternalRep (..),
  StringTransformFn,
) where



import qualified Data.Map as M



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
  | MkLensFields
  | MkEncodeJson
  | MkDecodeJson
  | MkRequestable
  | MkRespondable
  | MkIsForeign
  | MkShow
  | MkNone
  deriving (Show)



data MkG
  = MkGPurescriptImports
  | MkGHaskellImports
  deriving (Show)



type StringTransformFn = String -> String -> String



data InteropOptions = InteropOptions {
  fieldNameTransform :: StringTransformFn,
  jsonNameTransform :: StringTransformFn,
  jsonTagNameTransform :: StringTransformFn,
  spacingNL :: Int,
  spacingIndent :: Int,
  typeMap :: M.Map String String,
  lang :: Lang
}



data InternalRep
  = NewtypeRecIR String String [(String, String)]
  | NewtypeNormalIR String String
  | DataRecIR String String [(String, String)]
  | DataNormalIR String [(String, [String])]
  | TypeIR String String
  | EmptyIR
  deriving (Show)

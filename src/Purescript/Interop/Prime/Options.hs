{-# LANGUAGE RecordWildCards    #-}

module Purescript.Interop.Prime.Options (
  defaultOptions,
  defaultFieldNameTransform,
  defaultJsonNameTransform,
  defaultJsonTagNameTransform,

  defaultOptionsClean,
  defaultFieldNameTransformClean,
  defaultJsonNameTransformClean,
  defaultJsonTagNameTransformClean,

  defaultTypeMap,

  defaultPurescriptMks,
  defaultPurescriptMkGs,

  defaultHaskellMks,
  defaultHaskellMkGs
) where



import           Data.Char
import           Data.List
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Transform.UnCamel
import           Purescript.Interop.Prime.Misc
import           Purescript.Interop.Prime.Types



defaultOptions :: InteropOptions
defaultOptions = InteropOptions {
  fieldNameTransform = defaultFieldNameTransform,
  jsonNameTransform = defaultJsonNameTransform,
  jsonTagNameTransform = defaultJsonTagNameTransform,
  typeMap = defaultTypeMap,
  spacingNL = 2,
  spacingIndent = 2
}

defaultFieldNameTransform :: StringTransformFn
defaultFieldNameTransform _ s = s

defaultJsonNameTransform :: StringTransformFn
defaultJsonNameTransform _ s = s

defaultJsonTagNameTransform :: StringTransformFn
defaultJsonTagNameTransform _ s = s





defaultOptionsClean :: InteropOptions
defaultOptionsClean = InteropOptions {
  fieldNameTransform = defaultFieldNameTransformClean,
  jsonNameTransform = defaultJsonNameTransformClean,
  jsonTagNameTransform = defaultJsonTagNameTransformClean,
  typeMap = defaultTypeMap,
  spacingNL = 2,
  spacingIndent = 2
}

defaultFieldNameTransformClean :: StringTransformFn
defaultFieldNameTransformClean nb s =
  if isPrefixOf ftl s
    then firstToLower $ fromJust $ stripPrefix ftl s
    else s
  where
  ftl = firstToLower nb

defaultJsonNameTransformClean :: StringTransformFn
defaultJsonNameTransformClean nb s =
  if isPrefixOf ftl s
    then map toLower $ unCamelSource '_' $ fromJust $ stripPrefix ftl s
    else map toLower $ unCamelSource '_' s
  where
  ftl = firstToLower nb

defaultJsonTagNameTransformClean :: StringTransformFn
defaultJsonTagNameTransformClean _ s = s



defaultTypeMap :: M.Map String String
defaultTypeMap =
  M.fromList [
    ("Integer", "Int"),
    ("Double", "Number"),
    ("Float", "Number"),
    ("Bool", "Boolean"),
    ("Set", "Array"),
    ("List", "Array"),
    ("()", "Unit"),
    ("Text", "String"),
    ("ByteString", "String")
  ]



defaultPurescriptMks :: [Mk]
defaultPurescriptMks =
  [
    MkType
  , MkLens
  , MkMk
  , MkUnwrap
  , MkToJSON
  , MkFromJSON
  , MkLensFields
  , MkEncodeJson
  , MkDecodeJson
  , MkRequestable
  , MkRespondable
  , MkIsForeign
  , MkNone
  ]



defaultPurescriptMkGs :: [MkG]
defaultPurescriptMkGs =
  [ MkGPurescriptImports ]



defaultHaskellMks :: [Mk]
defaultHaskellMks =
  [
    MkToJSON
  , MkFromJSON
  ]



defaultHaskellMkGs :: [MkG]
defaultHaskellMkGs =
  [ MkGHaskellImports ]

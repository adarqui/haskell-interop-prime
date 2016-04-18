{-# LANGUAGE RecordWildCards    #-}

module Purescript.Interop.Prime.Options (
  defaultOptions,
  defaultOptionsHaskell,
  defaultOptionsPurescript,
  defaultFieldNameTransform,
  defaultJsonNameTransform,
  defaultJsonTagNameTransform,

  defaultOptionsClean,
  defaultOptionsCleanHaskell,
  defaultOptionsCleanPurescript,
  defaultFieldNameTransformClean,
  defaultJsonNameTransformClean,
  defaultJsonTagNameTransformClean,

  defaultTypeMap,
  defaultReservedMap,

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



langToMap :: Lang -> M.Map String String
langToMap LangHaskell    = M.empty
langToMap LangPurescript = defaultTypeMap



defaultOptionsPurescript :: FilePath -> InteropOptions
defaultOptionsPurescript = defaultOptions LangPurescript

defaultOptionsHaskell :: FilePath -> InteropOptions
defaultOptionsHaskell = defaultOptions LangHaskell

defaultOptions :: Lang -> FilePath -> InteropOptions
defaultOptions lang path = InteropOptions {
  fieldNameTransform = defaultFieldNameTransform,
  jsonNameTransform = defaultJsonNameTransform,
  jsonTagNameTransform = defaultJsonTagNameTransform,
  typeMap = langToMap lang,
  reservedMap = defaultReservedMap,
  spacingNL = 2,
  spacingIndent = 2,
  lang = lang,
  psDataToNewtype = True,
  filePath = path
}

defaultFieldNameTransform :: StringTransformFn
defaultFieldNameTransform _ s = s

defaultJsonNameTransform :: StringTransformFn
defaultJsonNameTransform _ s = s

defaultJsonTagNameTransform :: StringTransformFn
defaultJsonTagNameTransform _ s = s




defaultOptionsCleanPurescript :: FilePath -> InteropOptions
defaultOptionsCleanPurescript = defaultOptionsClean LangPurescript

defaultOptionsCleanHaskell :: FilePath -> InteropOptions
defaultOptionsCleanHaskell = defaultOptionsClean LangHaskell

defaultOptionsClean :: Lang -> FilePath -> InteropOptions
defaultOptionsClean lang path = InteropOptions {
  fieldNameTransform = defaultFieldNameTransformClean,
  jsonNameTransform = defaultJsonNameTransformClean,
  jsonTagNameTransform = defaultJsonTagNameTransformClean,
  typeMap = langToMap lang,
  reservedMap = defaultReservedMap,
  spacingNL = 2,
  spacingIndent = 2,
  lang = lang,
  psDataToNewtype = True,
  filePath = path
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
  M.fromList
    [ ("Integer", "Int")
    , ("Int64", "Int") -- TODO FIXME: Should use purescript-big-integers
    , ("Double", "Number")
    , ("Float", "Number")
    , ("Bool", "Boolean")
    , ("Set", "Array")
    , ("List", "Array")
    , ("()", "Unit")
    , ("Text", "String")
    , ("ByteString", "String")
    , ("UTCTime", "Date")
    ]



defaultReservedMap :: M.Map String String
defaultReservedMap =
  M.fromList
    [ ("data", "data'")
    , ("type", "type'")
    , ("class", "class'")
    , ("module", "module'")
    , ("let", "let'")
    ]



defaultPurescriptMks :: [Mk]
defaultPurescriptMks =
  [ MkType
  , MkLens
  , MkMk
  , MkUnwrap
  , MkToJSON
  , MkFromJSON
  , MkEncodeJson
  , MkDecodeJson
  , MkRequestable
  , MkRespondable
  , MkIsForeign
  , MkShow
  ]



defaultPurescriptMkGs :: String -> [MkG]
defaultPurescriptMkGs header =
  [ MkGPurescriptImports
  , MkGHeader header
  , MkGLensFields
  , MkGFooter "-- footer"
  ]



defaultHaskellMks :: [Mk]
defaultHaskellMks =
  [ MkToJSON
  , MkFromJSON
  ]



defaultHaskellMkGs :: String -> [MkG]
defaultHaskellMkGs header =
  [ MkGHaskellImports
  , MkGHeader header
  , MkGFooter "-- footer"
  ]

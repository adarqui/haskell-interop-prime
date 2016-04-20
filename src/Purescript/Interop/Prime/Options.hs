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

  defaultOptions_Purescript_adarqui,
  defaultOptions_Haskell_adarqui,

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

-- The logic for checking empty string after stripPrefix:
--
-- This becomes important when a field within a record is named exactly after the constructor,
-- which results in an empty name if you 'strip off' the constructor prefix. So, we keep the
-- original field name in this case.
--

defaultFieldNameTransformClean :: StringTransformFn
defaultFieldNameTransformClean nb s =
  if isPrefixOf ftl s
    then firstToLower fixed
    else s
  where
  ftl = firstToLower nb
  stripped = stripPrefix ftl s
  fixed =
    case stripped of
      Nothing -> s
      Just "" -> s
      Just v  -> v

defaultJsonNameTransformClean :: StringTransformFn
defaultJsonNameTransformClean nb s =
  if isPrefixOf ftl s
    then map toLower $ unCamelSource '_' fixed
    else map toLower $ unCamelSource '_' s
  where
  ftl = firstToLower nb
  stripped = stripPrefix ftl s
  fixed =
    case stripped of
      Nothing -> s
      Just "" -> s
      Just v  -> v

defaultJsonTagNameTransformClean :: StringTransformFn
defaultJsonTagNameTransformClean _ s = s





defaultOptions_Purescript_adarqui :: FilePath -> InteropOptions
defaultOptions_Purescript_adarqui = defaultOptionsCleanPurescript

defaultOptions_Haskell_adarqui :: FilePath -> InteropOptions
defaultOptions_Haskell_adarqui path =
  (defaultOptionsHaskell path) {
    jsonNameTransform = defaultJsonNameTransformClean
  }





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

{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleContexts   #-}

module Haskell.Interop.Prime.Options (
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
  defaultPurescriptApiMkGs,

  defaultHaskellMks,
  defaultHaskellMkGs,
  defaultHaskellApiMkGs
) where



import           Data.Char
import           Data.List
import qualified Data.Map                       as M
import           Data.Transform.UnCamel
import           Haskell.Interop.Prime.Misc
import           Haskell.Interop.Prime.Types



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
  filePath = path,
  debug = False
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
  filePath = path,
  debug = False
}

-- The logic for checking empty string after stripPrefix:
--
-- This becomes important when a field within a record is named exactly after the constructor,
-- which results in an empty name if you 'strip off' the constructor prefix. So, we keep the
-- original field name in this case.
--

defaultFieldNameTransformClean :: StringTransformFn
defaultFieldNameTransformClean nb s =
  if isPrefixOf lower_nb lower_s
    then firstToLower fixed
    else s
  where
  lower_nb = map toLower nb
  lower_s  = map toLower s
  ftl      = firstToLower nb
  stripped = drop (length lower_nb) s
  fixed    =
    case stripped of
      "" -> s
      v  -> v

defaultJsonNameTransformClean :: StringTransformFn
defaultJsonNameTransformClean nb s =
  if isPrefixOf lower_nb lower_s
    then dropSuffix $ map toLower $ unCamelSource '_' fixed
    else dropSuffix $ map toLower $ unCamelSource '_' s
  where
  lower_nb = map toLower nb
  lower_s  = map toLower s
  ftl      = firstToLower nb
  stripped = drop (length lower_nb) s
  fixed    =
    case stripped of
      "" -> s
      v  -> v
  -- this is somewhat hacky:
  -- if a json tag ends in _p, we assume it's part of the reserved map, ie, "data_p" == dataP
  -- so trim the _p off of the end so that we don't have to send json tags with _p from the server side
  -- this could be done in Template.hs, but i'd rather just make it user customizable for now.
  -- thnx.
  dropSuffix s =
    if isSuffixOf "_p" s
      then take (length s - 2) s
      else s

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
    [ ("data", "dataP")
    , ("type", "typeP")
    , ("class", "classP")
    , ("module", "moduleP")
    , ("let", "letP")
    ]



defaultPurescriptMks :: [Mk]
defaultPurescriptMks =
  [ MkType
  , MkTypeRows "R"
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
  , MkEq
  ]



defaultPurescriptMkGs :: String -> [MkG]
defaultPurescriptMkGs header =
  [ MkGPurescriptImports
  , MkGHeader header
  , MkGLensFields
  , MkGFooter "-- footer"
  ]



defaultPurescriptApiMkGs :: String -> [MkG]
defaultPurescriptApiMkGs header =
  [ MkGPurescriptApiImports
  , MkGHeader header
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



defaultHaskellApiMkGs :: String -> [MkG]
defaultHaskellApiMkGs header =
  [ MkGHaskellApiImports
  , MkGHeader header
  , MkGFooter "-- footer"
  ]

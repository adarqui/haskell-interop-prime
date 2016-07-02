{-# LANGUAGE TemplateHaskell #-}

module Haskell.Interop.Prime.Test where



import           Haskell.Interop.Prime
import           Haskell.Interop.Prime.Test.Internal
import           Haskell.Interop.Prime.Test.Types



mkExports
  (Options
    (defaultOptionsPurescript "/tmp/Interop.purs")
    (defaultPurescriptMkGs "module Interop where")
    (defaultOptionsHaskell "/tmp/Interop.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop"))
  [ (''Session,       defaultPurescriptMks, defaultHaskellMks)
  , (''SumType,       defaultPurescriptMks, defaultHaskellMks)
  , (''BigRecord,     defaultPurescriptMks, defaultHaskellMks)
  , (''FakeUTCTime,   defaultPurescriptMks, defaultHaskellMks)
  , (''User,          defaultPurescriptMks, defaultHaskellMks)
  , (''UserRequest,   defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponse,  defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponses, defaultPurescriptMks, defaultHaskellMks)
  , (''DateMaybe,     defaultPurescriptMks, defaultHaskellMks)
  , (''Text,          defaultPurescriptMks, defaultHaskellMks)
  , (''TextMaybe,     defaultPurescriptMks, defaultHaskellMks)
  , (''NestedList,    defaultPurescriptMks, defaultHaskellMks)
  , (''FunkyRecord,   defaultPurescriptMks, defaultHaskellMks)
  , (''FUnkyRecordP,  defaultPurescriptMks, defaultHaskellMks)
  ]



mkConvert
  (Options
    (defaultOptionsPurescript "/tmp/Interop.Convert.purs")
    (defaultPurescriptMkGs "module Interop.Convert where")
    (defaultOptionsHaskell "/tmp/Interop.Convert.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop.Convert"))
  [ (''UserRequest, ''UserResponse)
  , (''UserResponse, ''UserRequest) ]



--
-- Clean
--

mkExports
  (Options
    (defaultOptionsCleanPurescript "/tmp/Interop.Clean.purs")
    (defaultPurescriptMkGs "module Interop.Clean where")
    (defaultOptionsCleanHaskell "/tmp/Interop.Clean.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop.Clean"))
  [ (''Session,       defaultPurescriptMks, defaultHaskellMks)
  , (''SumType,       defaultPurescriptMks, defaultHaskellMks)
  , (''BigRecord,     defaultPurescriptMks, defaultHaskellMks)
  , (''FakeUTCTime,   defaultPurescriptMks, defaultHaskellMks)
  , (''User,          defaultPurescriptMks, defaultHaskellMks)
  , (''UserRequest,   defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponse,  defaultPurescriptMks, defaultHaskellMks)
  , (''UserResponses, defaultPurescriptMks, defaultHaskellMks)
  , (''DateMaybe,     defaultPurescriptMks, defaultHaskellMks)
  , (''Text,          defaultPurescriptMks, defaultHaskellMks)
  , (''TextMaybe,     defaultPurescriptMks, defaultHaskellMks)
  , (''NestedList,    defaultPurescriptMks, defaultHaskellMks)
  , (''FunkyRecord,   defaultPurescriptMks, defaultHaskellMks)
  , (''FUnkyRecordP,  defaultPurescriptMks, defaultHaskellMks)
  ]



mkConvert
  (Options
    (defaultOptionsCleanPurescript "/tmp/Interop.Convert.Clean.purs")
    (defaultPurescriptMkGs "module Interop.Convert.Clean where")
    (defaultOptionsCleanHaskell "/tmp/Interop.Convert.Clean.hs")
    (defaultHaskellMkGs $ tplTestHeader "Interop.Convert.Clean"))
  [ (''UserRequest, ''UserResponse)
  , (''UserResponse, ''UserRequest) ]



mkApi
  (Options
    ((defaultOptionsCleanPurescript "/tmp/Interop.Api.purs") { debug = True })
    (defaultPurescriptApiMkGs "module Interop.Api where")
    ((defaultOptionsCleanHaskell "/tmp/Interop.Api.hs" ) { debug = True })
    (defaultHaskellApiMkGs $ tplTestHeader "Interop.Api"))
  ''ApplicationError
  apiSpec_TH

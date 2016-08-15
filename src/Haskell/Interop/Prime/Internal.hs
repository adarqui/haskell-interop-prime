{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS -ddump-splices        #-}
{-# OPTIONS -fno-warn-orphans     #-}

module Haskell.Interop.Prime.Internal (
  mkExports
) where



import           Control.Monad                  (foldM, forM)
import           Control.Monad.Trans.RWS        (asks, evalRWS, gets, put)
import           Data.List                      (intercalate, nub)
import           Data.Maybe                     (catMaybes)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude

import           Haskell.Interop.Prime.Misc
import           Haskell.Interop.Prime.Shared
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



instance Lift Type where
  lift (ConT n)   = [| ConT (mkName nstr) |] where nstr = show n
  lift (AppT a b) = [| AppT a b |]
  lift (TupleT x) = [| TupleT x |]
  lift (ListT)    = [| ListT |]
  lift _          = error "unsupported"



opts_ir :: ExportT (InteropOptions, InternalRep)
opts_ir = do
  opts <- asks irInterop
  ir   <- gets isRep
  pure (opts, ir)



buildType :: ExportT (Maybe String)
buildType = buildTypeWith []



buildTypeWith :: [MkTypeOpts] -> ExportT (Maybe String)
buildTypeWith type_opts = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplNewtypeRecord type_opts opts base constr fields
      DataRecIR base constr fields    -> Just $ tplDataRecord type_opts opts base constr fields
      DataNormalIR base fields        -> Just $ tplDataNormal type_opts opts base fields
      TypeIR base vars type_          -> Just $ tplType opts base vars type_
      _                               -> Nothing



buildTypeRows :: String -> ExportT (Maybe String)
buildTypeRows suffix = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base _ fields -> Just $ tplRows opts suffix base fields
      DataRecIR base _ fields    -> Just $ tplRows opts suffix base fields
      _                          -> Nothing



buildLens :: ExportT (Maybe String)
buildLens = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplLensP opts base constr fields
      DataRecIR base constr fields    -> Just $ tplLensP opts base constr fields
      _                               -> Nothing



buildMk :: ExportT (Maybe String)
buildMk = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplMk opts base constr fields
      DataRecIR base constr fields    -> Just $ tplMk opts base constr fields
      _                               -> Nothing



buildUnwrap :: ExportT (Maybe String)
buildUnwrap = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplUnwrap opts base constr fields
      DataRecIR base constr fields    -> Just $ tplUnwrap opts base constr fields
      _                               -> Nothing



buildToJSON :: ExportT (Maybe String)
buildToJSON = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplToJSON_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplToJSON_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplToJSON_SumType opts base fields
      _                               -> Nothing



buildFromJSON :: ExportT (Maybe String)
buildFromJSON = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplFromJSON_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplFromJSON_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplFromJSON_SumType opts base fields
      _                               -> Nothing



buildEncodeJson :: ExportT (Maybe String)
buildEncodeJson = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplEncodeJson_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplEncodeJson_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplEncodeJson_SumType opts base fields
      _                               -> Nothing



buildDecodeJson :: ExportT (Maybe String)
buildDecodeJson = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplDecodeJson_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplDecodeJson_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplDecodeJson_SumType opts base fields
      _                               -> Nothing



buildRequestable :: ExportT (Maybe String)
buildRequestable = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base _ _ -> Just $ tplRequestable opts base
      DataRecIR base _ _    -> Just $ tplRequestable opts base
      DataNormalIR base _   -> Just $ tplRequestable opts base
      _                     -> Nothing



buildRespondable :: ExportT (Maybe String)
buildRespondable = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplRespondable_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplRespondable_Record opts base constr fields
      DataNormalIR base vars          -> Just $ tplRespondable_SumType opts base vars
      _                     -> Nothing



buildIsForeign :: ExportT (Maybe String)
buildIsForeign = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplIsForeign_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplIsForeign_Record opts base constr fields
      DataNormalIR base vars          -> Just $ tplIsForeign_SumType opts base vars
      _                               -> Nothing



buildShow :: ExportT (Maybe String)
buildShow = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplShow_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplShow_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplShow_SumType opts base fields
      _                               -> Nothing



buildRead :: ExportT (Maybe String)
buildRead = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      DataNormalIR base fields        ->
        -- **WARNING** We only support empty constructors for now
        if (sum (map (length . snd) fields) == 0)
          then Just $ tplRead_SumType opts base fields
          else Nothing
      _                               -> Nothing



buildEq :: ExportT (Maybe String)
buildEq = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplEq_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplEq_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplEq_SumType opts base fields
      _                               -> Nothing



buildQueryParam :: ExportT (Maybe String)
buildQueryParam = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      DataNormalIR base fields        ->
        -- **WARNING** We only support sum types according to: Data Blah = Param1 Type1 | Param2 Type2 ...
        if (sum (map (length . snd) fields) == (length fields))
          then Just $ tplQueryParam_SumType opts base fields
          else Nothing
      _                               -> Nothing



buildDefault :: String -> ExportT (Maybe String)
buildDefault default_value = do
  (opts, ir) <- opts_ir
  pure $
    case ir of
      NewtypeRecIR base _ _ -> Just $ tplDefault opts base default_value
      DataRecIR base _ _    -> Just $ tplDefault opts base default_value
      DataNormalIR base _   -> Just $ tplDefault opts base default_value
      _                     -> Nothing



runMk :: Mk -> ExportT (Maybe String)
runMk mk = do
  case mk of
    MkType              -> buildType
    MkTypeRows suffix   -> buildTypeRows suffix
    MkTypeWith typeopts -> buildTypeWith typeopts
    MkLens              -> buildLens
    MkMk                -> buildMk
    MkUnwrap            -> buildUnwrap
    MkToJSON            -> buildToJSON
    MkFromJSON          -> buildFromJSON
    MkEncodeJson        -> buildEncodeJson
    MkDecodeJson        -> buildDecodeJson
    MkRequestable       -> buildRequestable
    MkRespondable       -> buildRespondable
    MkIsForeign         -> buildIsForeign
    MkShow              -> buildShow
    MkRead              -> buildRead
    MkEq                -> buildEq
    MkQueryParam        -> buildQueryParam
    MkDefault value     -> buildDefault value




-- | This is the meat and potatoes. It exports types to a file
--
mkExports :: Options -> [(Name, [Mk], [Mk])] -> Q [Dec]
mkExports Options{..} nmm = do

  ir_ps <- forM (nub nmm) $ (\(t, psMks, _) -> do
      TyConI dec <- reify t
      pure (buildInternalRep psInterop dec, nub $ psMks)
    )

  ir_hs <- forM (nub nmm) $ (\(t, _, hsMks) -> do
      TyConI dec <- reify t
      pure $ (buildInternalRep hsInterop dec, nub $ hsMks)
    )

  let
    ps_fields = tplBuildFields psInterop $ map fst ir_ps
    (ps, _) = evalRWS (mkExports' psInterop psMkGs ir_ps) (InteropReader psInterop ps_fields) undefined
    (hs, _) = evalRWS (mkExports' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) undefined

  runIO $ persistResults psInterop ps
  runIO $ persistResults hsInterop hs

  pure []



-- | Builds everything out, from the mk's to the mkg's
-- and leaves us with a string representation of a module.
--
mkExports' :: InteropOptions -> [MkG] -> [(InternalRep, [Mk])] -> ExportT String
mkExports' InteropOptions{..} mkgs xs = do

  -- build a list of results of applying each mk to each Dec (type)
  mks <-
    mapM (\(ir,mks) -> do
        -- build*'s need ir in state
        put $ InteropState ir
        mapM (\mk -> do
            runMk mk
          ) mks
      ) xs

  -- fold over the stringified module, adding imports, headers, footers etc
  foldM
    (\acc mkg -> do
      r <- runMkG mkg acc
      case r of
        Nothing -> pure acc
        Just r' -> pure r'
    )
    (intercalate (newlines spacingNL) $ catMaybes $ concat mks)
    mkgs

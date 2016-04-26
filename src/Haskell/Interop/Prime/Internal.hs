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



import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Haskell.Interop.Prime.Misc
import           Haskell.Interop.Prime.Shared
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



instance Lift Type where
  lift (ConT n) = [| ConT (mkName nstr) |] where nstr = show n
  lift (AppT a b) = [| AppT a b |]
  lift (TupleT x) = [| TupleT x |]
  lift (ListT) = [| ListT |]



opts_ir :: ExportT (InteropOptions, InternalRep)
opts_ir = do
  opts <- asks irInterop
  ir   <- gets isRep
  return (opts, ir)



buildType :: ExportT (Maybe String)
buildType = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplNewtypeRecord opts base constr fields
      DataRecIR base constr fields    -> Just $ tplDataRecord opts base constr fields
      DataNormalIR base fields        -> Just $ tplDataNormal opts base fields
      TypeIR base type_               -> Just $ tplType opts base type_
      _                               -> Nothing



buildLens :: ExportT (Maybe String)
buildLens = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplLensP opts base constr fields
      DataRecIR base constr fields    -> Just $ tplLensP opts base constr fields
      _                               -> Nothing



buildMk :: ExportT (Maybe String)
buildMk = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplMk opts base constr fields
      DataRecIR base constr fields    -> Just $ tplMk opts base constr fields
      _                               -> Nothing



buildUnwrap :: ExportT (Maybe String)
buildUnwrap = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr _ -> Just $ tplUnwrap opts base constr
      DataRecIR base constr _    -> Just $ tplUnwrap opts base constr
      _                          -> Nothing



buildToJSON :: ExportT (Maybe String)
buildToJSON = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplToJSON_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplToJSON_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplToJSON_SumType opts base fields
      _                               -> Nothing



buildFromJSON :: ExportT (Maybe String)
buildFromJSON = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplFromJSON_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplFromJSON_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplFromJSON_SumType opts base fields
      _                               -> Nothing



buildEncodeJson :: ExportT (Maybe String)
buildEncodeJson = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplEncodeJson_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplEncodeJson_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplEncodeJson_SumType opts base fields
      _                               -> Nothing



buildDecodeJson :: ExportT (Maybe String)
buildDecodeJson = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base constr fields -> Just $ tplDecodeJson_Record opts base constr fields
      DataRecIR base constr fields    -> Just $ tplDecodeJson_Record opts base constr fields
      DataNormalIR base fields        -> Just $ tplDecodeJson_SumType opts base fields
      _                               -> Nothing



buildRequestable :: ExportT (Maybe String)
buildRequestable = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base _ _ -> Just $ tplRequestable opts base
      DataRecIR base _ _    -> Just $ tplRequestable opts base
      DataNormalIR base _   -> Just $ tplRequestable opts base
      _                     -> Nothing



buildRespondable :: ExportT (Maybe String)
buildRespondable = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base _ _ -> Just $ tplRespondable opts base
      DataRecIR base _ _    -> Just $ tplRespondable opts base
      DataNormalIR base _   -> Just $ tplRespondable opts base
      _                     -> Nothing



buildIsForeign :: ExportT (Maybe String)
buildIsForeign = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR base _ _ -> Just $ tplIsForeign opts base
      DataRecIR base _ _    -> Just $ tplIsForeign opts base
      DataNormalIR base _   -> Just $ tplIsForeign opts base
      _                     -> Nothing



buildShow :: ExportT (Maybe String)
buildShow = do
  (opts, ir) <- opts_ir
  return $
    case ir of
      NewtypeRecIR _ _ _       -> Nothing
      DataRecIR _ _ _          -> Nothing
      DataNormalIR base fields -> Just $ tplShow_SumType opts base fields
      _                        -> Nothing



runMk :: Mk -> ExportT (Maybe String)
runMk mk = do
  case mk of
    MkType              -> buildType
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




-- | This is the meat and potatoes. It exports types to a files
--
mkExports :: Options -> [(Name, [Mk], [Mk])] -> Q [Dec]
mkExports Options{..} nmm = do

  ir_ps <- forM nmm $ (\(t, psMks, _) -> do
      TyConI dec <- reify t
      return (buildInternalRep psInterop dec, psMks)
    )

  ir_hs <- forM nmm $ (\(t, _, hsMks) -> do
      TyConI dec <- reify t
      return $ (buildInternalRep hsInterop dec, hsMks)
    )

  let
    ps_fields = tplBuildFields psInterop $ map fst ir_ps
    (ps, _) = evalRWS (mkExports' psInterop psMkGs ir_ps) (InteropReader psInterop ps_fields) undefined
    (hs, _) = evalRWS (mkExports' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) undefined

  runIO $ persistResults psInterop ps
  runIO $ persistResults hsInterop hs

  return []



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
        Nothing -> return acc
        Just r' -> return r'
    )
    (intercalate (newlines spacingNL) $ catMaybes $ concat mks)
    mkgs



-- | Build the internel representation
--
buildInternalRep :: InteropOptions -> Dec -> InternalRep
buildInternalRep opts@InteropOptions{..} dec =

  parseInternalRep dec

  where

  parseInternalRep (NewtypeD _ n _ con _) = mkConNewtypeIR (nameBase n) con
  parseInternalRep (DataD _ n _ cons _) =
    case (head cons) of
      (RecC n' vars) -> DataRecIR (nameBase n) (nameBase n') (map (mkVarIR (nameBase n)) vars)
      (NormalC _ _)  -> DataNormalIR (nameBase n) (map (mkConDataIR' (nameBase n)) cons)
      _              -> EmptyIR
  parseInternalRep (TySynD n _ t) = TypeIR (nameBase n) (mkTypeIR opts t)
  parseInternalRep _ = EmptyIR

  mkConNewtypeIR nb (RecC n vars) = NewtypeRecIR nb (nameBase n) (map (mkVarIR nb) vars)
  mkConNewtypeIR _ (NormalC n vars) = NewtypeNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
  mkConNewtypeIR _ _ = EmptyIR

  mkConDataIR' _ (NormalC n vars) = (nameBase n, map mkVarIR' vars)
  mkConDataIR' _ _ = ("",[])

  mkVarIR nb (n, _, t) = (tplBuildField opts nb (nameBase n), mkTypeIR opts t)

  mkVarIR' (_, t) = mkTypeIR opts t

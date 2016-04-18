{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS -ddump-splices #-}
{-# OPTIONS -fno-warn-orphans #-}

module Purescript.Interop.Prime.Internal (
  mkExports
) where



import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.Map                          as M
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Purescript.Interop.Prime.Misc
import           Purescript.Interop.Prime.Template
import           Purescript.Interop.Prime.Types
import Control.Monad.Trans.RWS



instance Lift Type where
  lift (ConT n) = [| ConT (mkName nstr) |] where nstr = show n
  lift (AppT a b) = [| AppT a b |]
  lift (TupleT x) = [| TupleT x |]
  lift (ListT) = [| ListT |]



buildType :: InteropOptions -> InternalRep -> Maybe String
buildType opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplNewtypeRecord opts base constr fields
    DataRecIR base constr fields -> Just $ tplDataRecord opts base constr fields
    DataNormalIR base fields -> Just $ tplDataNormal opts base fields
    TypeIR base type_ -> Just $ tplType opts base type_
    _ -> Nothing



buildLens :: InteropOptions -> InternalRep -> Maybe String
buildLens opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplLensP opts base constr fields
    DataRecIR base constr fields -> Just $ tplLensP opts base constr fields
    _ -> Nothing



buildMk :: InteropOptions -> InternalRep -> Maybe String
buildMk opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplMk opts base constr fields
    DataRecIR base constr fields -> Just $ tplMk opts base constr fields
    _ -> Nothing



buildUnwrap :: InteropOptions -> InternalRep -> Maybe String
buildUnwrap opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr _ -> Just $ tplUnwrap opts base constr
    DataRecIR base constr _ -> Just $ tplUnwrap opts base constr
    _ -> Nothing



buildToJSON :: InteropOptions -> InternalRep -> Maybe String
buildToJSON opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplToJSON_Record opts base constr fields
    DataRecIR base constr fields -> Just $ tplToJSON_Record opts base constr fields
    DataNormalIR base fields -> Just $ tplToJSON_SumType opts base fields
    _ -> Nothing



buildFromJSON :: InteropOptions -> InternalRep -> Maybe String
buildFromJSON opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplFromJSON_Record opts base constr fields
    DataRecIR base constr fields -> Just $ tplFromJSON_Record opts base constr fields
    DataNormalIR base fields -> Just $ tplFromJSON_SumType opts base fields
    _ -> Nothing



buildEncodeJson :: InteropOptions -> InternalRep -> Maybe String
buildEncodeJson opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplEncodeJson_Record opts base constr fields
    DataRecIR base constr fields -> Just $ tplEncodeJson_Record opts base constr fields
    DataNormalIR base fields -> Just $ tplEncodeJson_SumType opts base fields
    _ -> Nothing



buildDecodeJson :: InteropOptions -> InternalRep -> Maybe String
buildDecodeJson opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base constr fields -> Just $ tplDecodeJson_Record opts base constr fields
    DataRecIR base constr fields -> Just $ tplDecodeJson_Record opts base constr fields
    DataNormalIR base fields -> Just $ tplDecodeJson_SumType opts base fields
    _ -> Nothing



buildRequestable :: InteropOptions -> InternalRep -> Maybe String
buildRequestable opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base _ _ -> Just $ tplRequestable opts base
    DataRecIR base _ _    -> Just $ tplRequestable opts base
    DataNormalIR base _   -> Just $ tplRequestable opts base
    _ -> Nothing



buildRespondable :: InteropOptions -> InternalRep -> Maybe String
buildRespondable opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base _ _ -> Just $ tplRespondable opts base
    DataRecIR base _ _    -> Just $ tplRespondable opts base
    DataNormalIR base _   -> Just $ tplRespondable opts base
    _ -> Nothing



buildIsForeign :: InteropOptions -> InternalRep -> Maybe String
buildIsForeign opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR base _ _ -> Just $ tplIsForeign opts base
    DataRecIR base _ _    -> Just $ tplIsForeign opts base
    DataNormalIR base _   -> Just $ tplIsForeign opts base
    _ -> Nothing



buildShow :: InteropOptions -> InternalRep -> Maybe String
buildShow opts@InteropOptions{..} ir =
  case ir of
    NewtypeRecIR _ _ _ -> Nothing
    DataRecIR _ _ _ -> Nothing
    DataNormalIR base fields -> Just $ tplShow_SumType opts base fields
    _ -> Nothing



runMk :: InteropOptions -> InternalRep -> Mk -> Maybe String
runMk opts@InteropOptions{..} ir mk =
  case mk of
    MkType              -> buildType opts ir
    MkLens              -> buildLens opts ir
    MkLensFields        -> Nothing
    MkMk                -> buildMk opts ir
    MkUnwrap            -> buildUnwrap opts ir
    MkToJSON            -> buildToJSON opts ir
    MkFromJSON          -> buildFromJSON opts ir
    MkEncodeJson        -> buildEncodeJson opts ir
    MkDecodeJson        -> buildDecodeJson opts ir
    MkRequestable       -> buildRequestable opts ir
    MkRespondable       -> buildRespondable opts ir
    MkIsForeign         -> buildIsForeign opts ir
    MkShow              -> buildShow opts ir



runMkG :: InteropOptions -> MkG -> String -> Maybe String
runMkG InteropOptions{..} mkg s =
  case mkg of
    MkGPurescriptImports -> Just $ tplPurescriptImports s
    MkGHaskellImports    -> Just $ tplHaskellImports s
    MkGHeader header     -> Just $ tplHeader header s
    MkGFooter footer     -> Just $ tplFooter footer s



-- | This is the meat and potatoes. It exports types to a files
--
mkExports :: Options -> [(Name, [Mk], [Mk])] -> Q [Dec]
mkExports opts@Options{..} nmm = do

  ir_ps <- forM nmm $ (\(t, psMks, hsMks) -> do
      TyConI dec <- reify t
      return (buildInternalRep psInterop dec, psMks)
    )

  ir_hs <- forM nmm $ (\(t, psMks, hsMks) -> do
      TyConI dec <- reify t
      return $ (buildInternalRep hsInterop dec, hsMks)
    )

  let
    ps_fields = buildFields psInterop $ map fst ir_ps
    (ps, _) = evalRWS (mkExports' psInterop psMkGs ir_ps) (InteropReader psInterop ps_fields) ()
    (hs, _) = evalRWS (mkExports' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) ()

  runIO $ persistInterop psInterop ps
  runIO $ persistInterop hsInterop hs

  return []



-- | Builds everything out, from the mk's to the mkg's
-- and leaves us with a string representation of a module.
--
mkExports' :: InteropOptions -> [MkG] -> [(InternalRep, [Mk])] -> ExportT String
mkExports' opts@InteropOptions{..} mkgs xs =

  return last_pass

  where
  mks         = map (\(ir,mks) -> map (\mk -> runMk opts ir mk) mks) xs
  first_pass  = concat $ intersperse (newlines spacingNL) $ catMaybes $ concat mks
  last_pass   =
    foldl'
      (\acc mkg -> let m = runMkG opts mkg acc in if m == Nothing then acc else fromJust m)
      first_pass
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
  parseInternalRep (TySynD n _ t) = TypeIR (nameBase n) (mkTypeIR t)
  parseInternalRep _ = EmptyIR

  mkConNewtypeIR nb (RecC n vars) = NewtypeRecIR nb (nameBase n) (map (mkVarIR nb) vars)
  mkConNewtypeIR _ (NormalC n vars) = NewtypeNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
  mkConNewtypeIR _ _ = EmptyIR

  mkConDataIR' _ (NormalC n vars) = (nameBase n, map mkVarIR' vars)
  mkConDataIR' _ _ = ("",[])

  mkVarIR nb (n, _, t) = (fieldNameTransform nb (nameBase n), mkTypeIR t)
  mkVarIR' (_, t) = mkTypeIR t

  mkTypeIR (ConT n) =
    case M.lookup (nameBase n) typeMap of
      Nothing -> nameBase n
      Just t  -> t
  mkTypeIR (VarT a) =
    let
      v = takeWhile (/= '_') $ nameBase a
    in case M.lookup v typeMap of
      Nothing -> v
      Just t  -> t
  mkTypeIR (AppT f x) = "(" ++ mkTypeIR f ++ " " ++ mkTypeIR x ++ ")"
  mkTypeIR (TupleT 0) = "Unit "
  mkTypeIR (TupleT 2) = "Tuple "
  mkTypeIR (TupleT n) = "Tuple" ++ show n ++ " "
  mkTypeIR ListT = "Array "
  mkTypeIR x     = show x




buildFields :: InteropOptions -> [InternalRep] -> [String]
buildFields opts@InteropOptions{..} ir =
  concat $ map go ir
  where
  go (NewtypeRecIR _ _ fields) = map fst fields
  go (DataRecIR _ _ fields)    = map fst fields
  go _                         = []



persistInterop :: InteropOptions -> String -> IO ()
persistInterop opts@InteropOptions{..} s = do

  writeFile filePath s

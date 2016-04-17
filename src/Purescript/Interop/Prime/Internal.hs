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
    MkRespondable       -> Nothing
    MkIsForeign         -> Nothing
    MkShow              -> buildShow opts ir



runMkG :: InteropOptions -> MkG -> String -> Maybe String
runMkG InteropOptions{..} mkg s =
  case mkg of
    MkGPurescriptImports -> Just $ tplPurescriptImports s
    MkGHaskellImports    -> Just $ tplHaskellImports s



-- | This is the meat and potatoes. It exports types to a files
--
mkExports :: InteropOptions -> Maybe (String, String, FilePath) -> [MkG] -> [(Name, [Mk])] -> Q [Dec]
mkExports opts@InteropOptions{..} out mkgs ts = do
  exports <- forM ts $ \(t, mks) -> do
    TyConI dec <- reify t
    let ir = parseInternalRep dec
    return $ concat $ intersperse (newlines spacingNL) $ catMaybes $ map (runMk opts ir) mks

  let
    exports'  =
      foldl'
        (\acc mkg -> let m = runMkG opts mkg acc in if m == Nothing then acc else fromJust m)
        (intercalate "\n" exports)
        mkgs
    handleAll :: SomeException -> IO ()
    handleAll _ = return ()

  case out of
    Just (header, footer, path) -> runIO $ handle handleAll $ writeFile path (header ++ "\n\n" ++ exports' ++ "\n\n" ++ footer)
    Nothing -> return ()

  return []

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

--      mkConDataIR nb (RecC n vars) = DataRecIR nb (nameBase n) (map (mkVarIR nb) vars)
--      mkConDataIR _ (NormalC n vars) = EmptyIR -- DataNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
--      mkConDataIR _ _ = EmptyIR

    mkConDataIR' _ (NormalC n vars) = (nameBase n, map mkVarIR' vars) -- DataNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
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

--      mkTyVarIR (PlainTV n) = nameBase n
--      mkTyVarIR (KindedTV n _) = nameBase n

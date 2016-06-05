{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS -ddump-splices #-}
{-# OPTIONS -fno-warn-orphans #-}

module Haskell.Interop.Prime.Shared (
  nameAndType,
  mkTypeIR,
  runMkG,
  persistResults,
  runDebug,
  buildInternalRep
) where



import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
import           Language.Haskell.TH
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



nameAndType :: InteropOptions -> String -> Name -> Q (String, String)
nameAndType opts param_name name = do
  TyConI dec <- reify name
  case dec of
    (TySynD _ _ t)         -> return (param_name, mkTypeIR opts t)
    _                      -> return (param_name, tplBuildType opts $ nameBase name)



mkTypeIR :: InteropOptions -> Type -> String
mkTypeIR opts@InteropOptions{..} type_ =
  case lang of
    LangPurescript -> mkTypeIR_Purescript opts type_
    LangHaskell    -> mkTypeIR_Haskell opts type_



mkTypeIR_Purescript :: InteropOptions -> Type -> String
mkTypeIR_Purescript opts (ConT n) = tplBuildType opts (nameBase n)
mkTypeIR_Purescript opts (VarT a) =
  let
    v = takeWhile (/= '_') $ nameBase a
  in
    tplBuildType opts v
mkTypeIR_Purescript opts (AppT f x) = "(" ++ mkTypeIR_Purescript opts f ++ " " ++ mkTypeIR_Purescript opts x ++ ")"
mkTypeIR_Purescript _ (TupleT 0)    = "Unit "
mkTypeIR_Purescript _ (TupleT 2)    = "Tuple "
mkTypeIR_Purescript _ (TupleT n)    = "Tuple" ++ show n ++ " "
mkTypeIR_Purescript _ ListT         = "Array "
mkTypeIR_Purescript _ x             = show x



mkTypeIR_Haskell :: InteropOptions -> Type -> String
mkTypeIR_Haskell opts (ConT n) = tplBuildType opts (nameBase n)
mkTypeIR_Haskell opts (VarT a) =
  let
    v = takeWhile (/= '_') $ nameBase a
  in
    tplBuildType opts v
mkTypeIR_Haskell opts (AppT ListT x) = "[" ++ mkTypeIR_Haskell opts x ++ "]"
mkTypeIR_Haskell opts (AppT f x)     = "(" ++ mkTypeIR_Haskell opts f ++ " " ++ mkTypeIR_Haskell opts x ++ ")"
mkTypeIR_Haskell _ (TupleT 0)        = "() "
mkTypeIR_Haskell _ (TupleT 2)        = "Tuple "
mkTypeIR_Haskell _ (TupleT n)        = "Tuple" ++ show n ++ " "
mkTypeIR_Haskell _ x                 = show x




runMkG :: MkG -> String -> ExportT (Maybe String)
runMkG mkg s = do
  opts   <- asks irInterop
  fields <- asks irFields
  return $
    case mkg of
      MkGPurescriptImports    -> Just $ tplPurescriptImports s
      MkGPurescriptApiImports -> Just $ tplApiImports opts s
      MkGHaskellImports       -> Just $ tplHaskellImports s
      MkGHaskellApiImports    -> Just $ tplApiImports opts s
      MkGLensFields           -> Just $ tplLensFields opts fields s
      MkGHeader header        -> Just $ tplHeader header s
      MkGFooter footer        -> Just $ tplFooter footer s



persistResults :: InteropOptions -> String -> IO ()
persistResults InteropOptions{..} s = do

  writeFile filePath s



runDebug :: InteropOptions -> IO a -> Q ()
runDebug InteropOptions{..} f = do
  if debug
    then void $ runIO $ f
    else return ()



-- | Build the internel representation of a type
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
  parseInternalRep (TySynD n vars t) = TypeIR (nameBase n) (tyVarBndrToList vars) (mkTypeIR opts t)
  parseInternalRep _ = EmptyIR

  mkConNewtypeIR nb (RecC n vars) = NewtypeRecIR nb (nameBase n) (map (mkVarIR nb) vars)
  mkConNewtypeIR _ (NormalC n vars) = NewtypeNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
  mkConNewtypeIR _ _ = EmptyIR

  mkConDataIR' _ (NormalC n vars) = (nameBase n, map mkVarIR' vars)
  mkConDataIR' _ _ = ("",[])

  mkVarIR nb (n, _, t) = (tplBuildField opts nb (nameBase n), mkTypeIR opts t)

  mkVarIR' (_, t) = mkTypeIR opts t

  tyVarBndrToList vars = catMaybes $ map (\var -> go var) vars
    where
    go (PlainTV n)    = Just $ nameBase n
    go (KindedTV n _) = Just $ nameBase n

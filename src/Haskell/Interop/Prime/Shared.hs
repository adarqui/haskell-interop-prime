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



import           Control.Monad                  (void)
import           Control.Monad.Trans.RWS        (asks)
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
import           Data.Monoid                    ((<>))
import           Language.Haskell.TH
import           Prelude

import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



nameAndType :: InteropOptions -> String -> Name -> Q (String, String)
nameAndType opts param_name name = do
  TyConI dec <- reify name
  case dec of
    (TySynD _ _ t)         -> pure (param_name, mkTypeIR opts t)
    _                      -> pure (param_name, tplBuildType opts $ nameBase name)



mkTypeIR :: InteropOptions -> Type -> String
mkTypeIR opts@InteropOptions{..} type_ =
  case lang of
    LangPurescript -> mkTypeIR_Purescript opts type_
    LangHaskell    -> mkTypeIR_Haskell opts type_



mkTypeIR_Purescript :: InteropOptions -> Type -> String
mkTypeIR_Purescript opts ty =
  tplBuildType opts $
    case ty of
      ConT n   -> nameBase n
      VarT a   -> takeWhile (/= '_') $ nameBase a
      AppT f x -> "(" <> mkTypeIR_Purescript opts f <> " " <> mkTypeIR_Purescript opts x <> ")"
      TupleT 0 -> "Unit "
      TupleT 2 -> "Tuple "
      TupleT n -> "Tuple" <> show n <> " "
      ListT    -> "Array "
      x        -> show x



mkTypeIR_Haskell :: InteropOptions -> Type -> String
mkTypeIR_Haskell opts ty =
  tplBuildType opts $
    case ty of
      ConT n       -> nameBase n
      VarT a       -> takeWhile (/= '_') $ nameBase a
      AppT ListT x -> "[" <> mkTypeIR_Haskell opts x <> "]"
      AppT f x     -> "(" <> mkTypeIR_Haskell opts f <> " " <> mkTypeIR_Haskell opts x <> ")"
      TupleT 0     -> "() "
      TupleT 2     -> "(,) "
      TupleT n     -> "(" <> replicate n ',' <> ")"
      x            -> show x



runMkG :: MkG -> String -> ExportT (Maybe String)
runMkG mkg s = do
  opts   <- asks irInterop
  fields <- asks irFields
  pure $
    case mkg of
      MkGPurescriptImports          -> Just $ tplImports opts s
      MkGPurescriptApiImports       -> Just $ tplApiImports opts s
      MkGPurescriptApiStringImports -> Just $ tplApiStringImports opts s
      MkGPurescriptConvertImports   -> Just $ tplConvertImports opts s
      MkGHaskellImports             -> Just $ tplImports opts s
      MkGHaskellApiImports          -> Just $ tplApiImports opts s
      MkGHaskellApiStringImports    -> Just $ tplApiStringImports opts s
      MkGHaskellConvertImports      -> Just $ tplImports opts s
      MkGLensFields                 -> Just $ tplLensFields opts fields s
      MkGHeader header              -> Just $ tplHeader header s
      MkGFooter footer              -> Just $ tplFooter footer s



persistResults :: InteropOptions -> String -> IO ()
persistResults InteropOptions{..} s = do

  writeFile filePath s



runDebug :: InteropOptions -> IO a -> Q ()
runDebug InteropOptions{..} f = do
  if debug
    then void $ runIO $ f
    else pure ()



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

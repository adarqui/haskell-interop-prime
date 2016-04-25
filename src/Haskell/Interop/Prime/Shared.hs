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
  persistResults,
  runDebug
) where



import           Control.Monad
import           Language.Haskell.TH
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



nameAndType :: InteropOptions -> String -> Name -> Q (String, String)
nameAndType opts param_name name = do
  TyConI dec <- reify name
  case dec of
    (TySynD _ _ t)         -> return (param_name, mkTypeIR opts t)
    _                      -> return (param_name, nameBase name)



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
mkTypeIR_Purescript _ (TupleT 0) = "Unit "
mkTypeIR_Purescript _ (TupleT 2) = "Tuple "
mkTypeIR_Purescript _ (TupleT n) = "Tuple" ++ show n ++ " "
mkTypeIR_Purescript _ ListT = "Array "
mkTypeIR_Purescript _ x     = show x



mkTypeIR_Haskell :: InteropOptions -> Type -> String
mkTypeIR_Haskell opts (ConT n) = tplBuildType opts (nameBase n)
mkTypeIR_Haskell opts (VarT a) =
  let
    v = takeWhile (/= '_') $ nameBase a
  in
    tplBuildType opts v
mkTypeIR_Haskell opts (AppT ListT x) = "[" ++ mkTypeIR_Haskell opts x ++ "]"
mkTypeIR_Haskell opts (AppT f x) = "(" ++ mkTypeIR_Haskell opts f ++ " " ++ mkTypeIR_Haskell opts x ++ ")"
mkTypeIR_Haskell _ (TupleT 0) = "() "
mkTypeIR_Haskell _ (TupleT 2) = "Tuple "
mkTypeIR_Haskell _ (TupleT n) = "Tuple" ++ show n ++ " "
mkTypeIR_Haskell _ x     = show x



persistResults :: InteropOptions -> String -> IO ()
persistResults InteropOptions{..} s = do

  writeFile filePath s



runDebug :: InteropOptions -> IO a -> Q ()
runDebug InteropOptions{..} f = do
  if debug
    then void $ runIO $ f
    else return ()

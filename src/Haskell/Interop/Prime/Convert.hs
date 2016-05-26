{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS -ddump-splices        #-}
{-# OPTIONS -fno-warn-orphans     #-}

module Haskell.Interop.Prime.Convert (
  mkConvert
) where



import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.List                      (intercalate)
import           Data.Maybe                     (catMaybes)
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



-- | This is the meat and potatoes. It exports types to a file
--
mkConvert :: Options -> [(Name, Name)] -> Q [Dec]
mkConvert Options{..} nn = do

  ir_ps <- forM nn $ (\(t1, t2) -> do
      TyConI dec1 <- reify t1
      TyConI dec2 <- reify t2
      return (buildInternalRep psInterop dec1, buildInternalRep psInterop dec2)
    )

  ir_hs <- forM nn $ (\(t1, t2) -> do
      TyConI dec1 <- reify t1
      TyConI dec2 <- reify t2
      return $ (buildInternalRep hsInterop dec1, buildInternalRep hsInterop dec2)
    )

  let
    (ps, _) = evalRWS (mkConvert' psInterop psMkGs ir_ps) (InteropReader psInterop []) undefined
    (hs, _) = evalRWS (mkConvert' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) undefined

  runIO $ persistResults psInterop ps
  runIO $ persistResults hsInterop hs

  return []



-- | Builds everything out, from the mk's to the mkg's
-- and leaves us with a string representation of a module.
--
mkConvert' :: InteropOptions -> [MkG] -> [(InternalRep, InternalRep)] -> ExportT String
mkConvert' InteropOptions{..} mkgs xs = do

  -- build a list of results of applying each mk to each Dec (type)
  converts <-
    mapM (\(ir1,ir2) -> do
      return $ [Just "hi"]
    ) xs

  -- fold over the stringified module, adding imports, headers, footers etc
  foldM
    (\acc mkg -> do
      r <- runMkG mkg acc
      case r of
        Nothing -> return acc
        Just r' -> return r'
    )
    (intercalate (newlines spacingNL) $ catMaybes $ concat converts)
    mkgs

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



import           Control.Monad                  (foldM, forM)
import           Control.Monad.Trans.RWS        (evalRWS)
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



-- | This is the meat and potatoes. It exports types to a file
--
mkConvert :: Options -> [(Name, Name)] -> Q [Dec]
mkConvert Options{..} nn = do

  ir_ps <- forM (nub nn) $ (\(t1, t2) -> do
      TyConI dec1 <- reify t1
      TyConI dec2 <- reify t2
      pure (buildInternalRep psInterop dec1, buildInternalRep psInterop dec2)
    )

  ir_hs <- forM nn $ (\(t1, t2) -> do
      TyConI dec1 <- reify t1
      TyConI dec2 <- reify t2
      pure $ (buildInternalRep hsInterop dec1, buildInternalRep hsInterop dec2)
    )

  let
    (ps, _) = evalRWS (mkConvert' psInterop psMkGs ir_ps) (InteropReader psInterop []) undefined
    (hs, _) = evalRWS (mkConvert' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) undefined

  runIO $ persistResults psInterop ps
  runIO $ persistResults hsInterop hs

  pure []



-- | Builds everything out, from the mk's to the mkg's
-- and leaves us with a string representation of a module.
--
mkConvert' :: InteropOptions -> [MkG] -> [(InternalRep, InternalRep)] -> ExportT String
mkConvert' opts@InteropOptions{..} mkgs xs = do

  -- build a list of results of applying each mk to each Dec (type)
  converts <-
    mapM (\(ir1,ir2) -> do

      let r = case (ir1,ir2) of
                (NewtypeRecIR base1 constr1 fields1, NewtypeRecIR base2 constr2 fields2) ->
                  Just $ tplConvertRecord opts (base1,constr1,fields1) (base2,constr2,fields2)
                (DataRecIR base1 constr1 fields1, DataRecIR base2 constr2 fields2)       ->
                  Just $ tplConvertRecord opts (base1,constr1,fields1) (base2,constr2,fields2)
                _                               -> Nothing

      pure [r]

    ) xs

  -- fold over the stringified module, adding imports, headers, footers etc
  foldM
    (\acc mkg -> do
      r <- runMkG mkg acc
      case r of
        Nothing -> pure acc
        Just r' -> pure r'
    )
    (intercalate (newlines spacingNL) $ catMaybes $ concat converts)
    mkgs

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS -ddump-splices #-}
{-# OPTIONS -fno-warn-orphans #-}

module Haskell.Interop.Prime.Api (
  mkApi
) where



import           Control.Monad
import           Control.Monad.Trans.RWS
import           Data.List
import qualified Data.Map                          as M
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Haskell.Interop.Prime.Misc
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types



default (String)



mkApi :: Options -> Api -> Q [Dec]
mkApi Options{..} Api{..} = do

  api_ps <- forM apiEntries $ (\api_entry -> do
      runIO $ print api_entry
      return $ tplPurescriptApiEntry psInterop api_entry
    )

  api_hs <- forM apiEntries $ (\api_entry -> do
      return $ tplHaskellApiEntry hsInterop api_entry
    )

  runIO $ mapM putStrLn api_ps

{-
  let
    (ps, _) = evalRWS (mkExports' psInterop psMkGs ir_ps) (InteropReader psInterop ps_fields) undefined
    (hs, _) = evalRWS (mkExports' hsInterop hsMkGs ir_hs) (InteropReader hsInterop []) undefined

  runIO $ persistInterop psInterop ps
  runIO $ persistInterop hsInterop hs
  -}

  return []



persistApi :: InteropOptions -> String -> IO ()
persistApi InteropOptions{..} s = do

  writeFile filePath s

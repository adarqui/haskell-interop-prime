{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS -ddump-splices        #-}
{-# OPTIONS -fno-warn-orphans     #-}

module Haskell.Interop.Prime.Api (
  mkApi
) where



import           Control.Monad                  (forM, foldM)
import           Control.Monad.Trans.RWS        (evalRWS)
import           Data.List                      (intercalate)
import           Haskell.Interop.Prime.Shared
import           Haskell.Interop.Prime.Template
import           Haskell.Interop.Prime.Types
import           Language.Haskell.TH



default (String)



buildMethods :: InteropOptions -> [ApiMethod_TH] -> Q [ApiMethod]
buildMethods opts = mapM (buildMethod opts)



buildMethod :: InteropOptions -> ApiMethod_TH -> Q ApiMethod
buildMethod opts@InteropOptions{..} api_method_th =
  case api_method_th of
    ApiGET_TH resp      -> ApiGET <$> (fmap snd $ type_ resp)
    ApiPOST_TH req resp -> ApiPOST <$> (fmap snd $ type_ req) <*> (fmap snd $ type_ resp)
    ApiPUT_TH req resp  -> ApiPUT <$> (fmap snd $ type_ req) <*> (fmap snd $ type_ resp)
    ApiDELETE_TH resp   -> ApiDELETE <$> (fmap snd $ type_ resp)
  where
  type_ = nameAndType opts "_"



buildParams :: InteropOptions -> [ApiParam_TH] -> Q [ApiParam]
buildParams opts = mapM (buildParam opts)



buildParam :: InteropOptions -> ApiParam_TH -> Q ApiParam
buildParam opts@InteropOptions{..} api_param_th =
  case api_param_th of
    Par_TH params         -> do
      params' <- mapM (\(p,t) -> nameAndType opts p t) params
      pure $ Par params'
    ParBy_TH param name   -> do
      (p, t) <- nameAndType opts param name
      pure $ ParBy p t
    ParBoth_TH args (p,t) ->
      ParBoth
      <$> mapM (\(p',t') -> nameAndType opts p' t') args
      <*> nameAndType opts p t
    ParNone_TH            -> pure ParNone



buildInternalApiRep :: InteropOptions -> ApiEntry_TH -> Q ApiEntry
buildInternalApiRep opts@InteropOptions{..} (ApiEntry_TH route params methods) =
  ApiEntry <$> (pure route) <*> (buildParams opts params) <*> (buildMethods opts methods)



mkApi :: Options -> Api_TH -> Q [Dec]
mkApi Options{..} api@Api_TH{..} = do

  mkApi' psInterop psMkGs api
  mkApi' hsInterop hsMkGs api

  pure []



mkApi' :: InteropOptions -> [MkG] -> Api_TH -> Q ()
mkApi' opts@InteropOptions{..} mkgs Api_TH{..} = do

  ir <- forM apiEntries_TH $ (\api_entry_th -> do
     buildInternalApiRep opts api_entry_th
    )

  let Api{..} = Api {
    apiPrefix = apiPrefix_TH,
    apiEntries = ir
  }

  result <- forM apiEntries $ (\api_entry -> do
      pure $ tplApiEntry opts api_entry
    )

  let
    intermediate_module = intercalate "\n" result
    (api_module, _)     =
      -- EEK
      evalRWS
        (
          foldM (\acc mkg -> do
              r <- runMkG mkg acc
              case r of
                Nothing -> pure acc
                Just r' -> pure r'
            )
            intermediate_module
            mkgs
        )
        (InteropReader opts [])
        undefined

  runDebug opts $ putStrLn api_module

  runIO $ persistResults opts api_module

  pure ()

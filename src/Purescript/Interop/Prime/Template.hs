{-# LANGUAGE RecordWildCards #-}

module Purescript.Interop.Prime.Template (
  tplType,
  tplLensP,
  tplNewtypeRecord,
  tplDataRecord,
  tplRecord,
  tplDataNormal,
  tplMk,
  tplUnwrap,
  tplToJSON_Record,
  tplFromJSON_Record,
  tplToJSON_SumType,
  tplFromJSON_SumType,
  tplPurescriptImports,
  tplHaskellImports
) where



import           Data.List
import           Purescript.Interop.Prime.Misc
import           Purescript.Interop.Prime.Types
import           Text.Printf



tplType :: InteropOptions -> String -> String -> String
tplType InteropOptions{..} base type_ =
  printf "type %s = %s\n" base type_



tplLensP :: InteropOptions -> String -> String -> [(String, String)] -> String
tplLensP InteropOptions{..} base constr fields =
     printf "_%s :: LensP %s {\n" base base
  ++ (concat $ intersperse ",\n" $ map (\(n,t) -> spaces spacingIndent ++ printf "%s :: %s" (fieldNameTransform base n) t) fields)
  ++ "\n}\n"
  ++ printf "_%s f (%s o) = %s <$> f o\n" base constr constr



tplNewtypeRecord :: InteropOptions -> String -> String -> [(String, String)] -> String
tplNewtypeRecord = tplRecord "newtype"



tplDataRecord :: InteropOptions -> String -> String -> [(String, String)] -> String
tplDataRecord = tplRecord "data"



tplRecord :: String -> InteropOptions -> String -> String -> [(String, String)] -> String
tplRecord type_ InteropOptions{..} base constr fields =
     printf "%s %s = %s {\n" type_ base constr
  ++ (concat $ intersperse ",\n" $ map (\(n,t) -> spaces spacingIndent ++ printf "%s :: %s" (fieldNameTransform base n) t) fields)
  ++ "\n}\n"



tplDataNormal :: InteropOptions -> String -> [(String, [String])] -> String
tplDataNormal InteropOptions{..} base fields =
     printf "data %s\n" base
  ++ spaces spacingIndent ++ "= "
  ++ (intercalate (spaces spacingIndent ++ "| ") $ map (\(n,t) -> printf "%s %s\n" n (intercalate " " t)) fields)
  ++ "\n"



tplMk :: InteropOptions -> String -> String -> [(String, String)] -> String
tplMk InteropOptions{..} base constr fields =
     printf "mk%s :: " base
  ++ (concat $ intersperse " -> " $ map (\(_,t) -> t) (fields ++ [("tmp",base)])) ++ "\n"
  ++ printf "mk%s " base
  ++ (concat $ intersperse " " $ map (\(n,_) -> fieldNameTransform base n) fields) ++ " =\n"
  ++ (spaces spacingIndent) ++ constr ++ "{"
  ++ (concat $ intersperse ", " $ map (\(n,_) -> fieldNameTransform base n) fields)
  ++ "}\n"



-- TODO FIXME: need to also generate a type sig
--
tplUnwrap :: InteropOptions -> String -> String -> String
tplUnwrap InteropOptions{..} base constr =
    printf "unwrap%s (%s r) = r" base constr



tplToJSON_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplToJSON_Record InteropOptions{..} base constr fields =
     printf "instance %sToJson :: ToJSON %s where\n" (firstToLower base) base
  ++ spaces si1 ++ printf "toJSON (%s v) = object $\n" constr
  ++ spaces si2 ++ printf "[ \"tag\" .= \"%s\"\n" base
  ++ (concat $ map (\(n,_) -> spaces si2 ++ printf ", \"%s\" .= v.%s\n" (jsonNameTransform base n) (fieldNameTransform base n)) fields)
  ++ spaces si2 ++ printf "]\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2



tplFromJSON_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplFromJSON_Record InteropOptions{..} base constr fields =
     printf "instance %sFromJson :: FromJSON %s where\n" (firstToLower base) base
  ++ spaces si1 ++ printf "parseJSON (JObject o) = do\n"
  ++ (concat $ map (\(n,_) -> spaces si2 ++ printf "%s <- o .: \"%s\"\n" (fieldNameTransform base n) (jsonNameTransform base n)) fields)
  ++ spaces si2 ++ printf "return $ %s {\n" constr
  ++ (concat $ intersperse ",\n" $ map (\(n,_) -> spaces si3 ++ printf "%s : %s" (fieldNameTransform base n) (fieldNameTransform base n)) fields)
  ++ "\n" ++ spaces si1 ++ "}\n"
  ++ spaces spacingIndent ++ printf "parseJSON x = fail $ \"Could not parse object: \" ++ show x\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2
  si3 = spacingIndent*3



tplToJSON_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplToJSON_SumType opts@InteropOptions{..} base fields =
     printf "instance %sToJson :: ToJSON %s where\n" (firstToLower base) base
  ++ (concat $ map (\(f,vars) -> tplToJSON_SumType_Field opts f vars) fields)



tplToJSON_SumType_Field :: InteropOptions -> String -> [String] -> String
tplToJSON_SumType_Field InteropOptions{..} field vars =
     spaces si1 ++ printf "toJSON (%s %s) = object $\n" field (intercalate " " vars')
  ++ spaces si2 ++ printf "[ \"tag\" .= \"%s\"\n" field
  ++
     (if null vars
        then spaces si2 ++ printf ", \"contents\" .= ([] :: Array String)\n"
        else spaces si2 ++ printf ", \"contents\" .= " ++ wrapContent vars (intercalate ", " (map ("toJSON " ++) vars')) ++ "\n")
  ++ spaces si2 ++ "]\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  vars' = vars_x $ length vars



tplFromJSON_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplFromJSON_SumType opts@InteropOptions{..} base fields =
     printf "instance %sFromJson :: FromJSON %s where\n" (firstToLower base) base
  ++ spaces si1 ++ "parseJSON (JObject o) = do\n"
  ++ spaces si2 ++ "tag <- o .: \"tag\"\n"
  ++ spaces si2 ++ "case tag of\n"
  ++ (concat $ map (\(f,vars) -> tplFromJSON_SumType_Field opts f vars) fields)
  ++ spaces spacingIndent ++ printf "parseJSON x = fail $ \"Could not parse object: \" ++ show x\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2



tplFromJSON_SumType_Field :: InteropOptions -> String -> [String] -> String
tplFromJSON_SumType_Field InteropOptions{..} field vars =
     spaces si1 ++ printf "\"%s\" -> do\n" field
  ++
     (if null vars
       then spaces si2 ++ printf "return $ %s\n" field
       else
            spaces si2 ++ wrapContent vars (intercalate ", " vars') ++ " <- o .: \"contents\"\n"
         ++ spaces si2 ++ printf "%s <$> %s" field (intercalate " <*> " (map ("parseJSON " ++) vars') ++ "\n"))
  ++ "\n"
  where
  si1 = spacingIndent*3
  si2 = spacingIndent*4
  vars' = vars_x $ length vars




tplPurescriptImports :: String -> String
tplPurescriptImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Data.JSON"
  , "import Data.Either"
  , "import Data.Maybe"
  , "import Data.List (List ())"
  , "import Data.Tuple"
  , "import Data.Set (Set ())"
  , "import Optic.Lens"
  , "import Optic.Core"
  , "import Control.Monad.Aff"
  , "import Prelude"
  , ""
  , ""
  ]) ++ s



tplHaskellImports :: String -> String
tplHaskellImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Data.Aeson"
  , ""
  , ""
  ]) ++ s

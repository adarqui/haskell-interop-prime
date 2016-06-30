{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}

module Haskell.Interop.Prime.Template (
  tplType,
  tplLensP,
  tplLensFields,
  tplNewtypeRecord,
  tplDataRecord,
  tplRecord,
  tplConvertRecord,
  tplConvertRecord_Purescript,
  tplConvertRecord_Haskell,
  tplRows,
  tplDataNormal,
  tplMk,
  tplUnwrap,
  tplToJSON_Record,
  tplFromJSON_Record,
  tplToJSON_SumType,
  tplFromJSON_SumType,
  tplEncodeJson_Record,
  tplDecodeJson_Record,
  tplEncodeJson_SumType,
  tplDecodeJson_SumType,
  tplRequestable,
  tplRespondable_Record,
  tplRespondable_SumType,
  tplIsForeign_Record,
  tplIsForeign_SumType,
  tplShow_Record,
  tplShow_SumType,
  tplEq_Record,
  tplEq_SumType,
  tplImports,
  tplPurescriptImports,
  tplHaskellImports,
  tplApiImports,
  tplPurescriptApiImports,
  tplHaskellApiImports,
  tplHeader,
  tplFooter,
  tplJObject,
  tplTestHeader,
  tplApiEntry,
  tplApiMethod_Prefix,
  tplApiMethod_ResultType,
  tplApiMethod_RequestType,
  tplApiParam_TypeSig,
  tplApiParam_Arguments,
  tplApiParam_ByName,
  tplBuildFields,
  tplBuildField,
  tplBuildType,
  tplArrows,
  tplArguments,
  trim
) where



import           Data.List
import qualified Data.Map                    as M
import           Data.Monoid
import           Haskell.Interop.Prime.Misc
import           Haskell.Interop.Prime.Types
import           Text.Printf



default (String)



tplType :: InteropOptions -> String -> [String] -> String -> String
tplType InteropOptions{..} base vars type_ =
  printf "type %s %s = %s\n" base (intercalate " " vars) type_



tplLensP :: InteropOptions -> String -> String -> [(String, String)] -> String
tplLensP InteropOptions{..} base constr fields =
     printf "_%s :: Lens' %s {\n" base base
  <> intercalateMap ",\n" (\(n,t) -> spaces spacingIndent <> printf "%s :: %s" (fieldNameTransform base n) t) fields
  <> "\n}\n"
  <> printf "_%s f (%s o) = %s <$> f o\n" base constr constr



tplLensFields :: InteropOptions -> [String] -> String -> String
tplLensFields InteropOptions{..} fields s =
  s <> (newlines spacingNL) <> (intercalateMap (newlines spacingNL) tplLensField fields)



tplLensField :: String -> String
tplLensField field =
     printf "%s_ :: forall b a r. Lens { %s :: a | r } { %s :: b | r } a b\n" field field field
  <> printf "%s_ f o = o { %s = _ } <$> f o.%s\n" field field field



tplNewtypeRecord :: InteropOptions -> String -> String -> [(String, String)] -> String
tplNewtypeRecord = tplRecord "newtype"



tplDataRecord :: InteropOptions -> String -> String -> [(String, String)] -> String
tplDataRecord opts@InteropOptions{..} =
  if psDataToNewtype
    then tplRecord "newtype" opts
    else tplRecord "data" opts



tplRecord :: String -> InteropOptions -> String -> String -> [(String, String)] -> String
tplRecord type_ InteropOptions{..} base constr fields =
     printf "%s %s = %s {\n" type_ base constr
  <> intercalateMap ",\n" (\(n,t) -> spaces spacingIndent <> printf "%s :: %s" (fieldNameTransform base n) t) fields
  <> "\n}\n"



-- | Converts 1 to 2
--
tplConvertRecord :: InteropOptions -> (String, String, [(String, String)]) -> (String, String, [(String, String)]) -> String
tplConvertRecord opts@InteropOptions{..} one two =
  case lang of
    LangPurescript -> tplConvertRecord_Purescript opts one two
    LangHaskell    -> tplConvertRecord_Haskell opts one two
    _              -> "tplConvertRecord not supported"



-- | Converts 1 to 2
--
tplConvertRecord_Purescript :: InteropOptions -> (String, String, [(String, String)]) -> (String, String, [(String, String)]) -> String
tplConvertRecord_Purescript InteropOptions{..} (base1,constr1,fields1) (base2,constr2,fields2) =
     printf "%s :: %s\n" fn_name (tplArrows arrows)
  <> printf "%s %s (%s o) =\n" fn_name (tplArguments args_diff) constr1
  <> spaces spacingIndent <> printf "%s {\n" constr2
  <> intercalateMap ",\n" (\(n,_) -> spaces (spacingIndent*2) <> printf "%s: %s" (fieldNameTransform constr2 n) (proper constr1 n)) fields2
  <> "\n"
  <> spaces spacingIndent <> "}\n"
  where
  fn_name          = firstToLower constr1 <> "To" <> constr2
  fields_union     = union fields1 fields2
  fields_intersect = intersect fields1 fields2
  fields_diff      = fields2 \\ fields1
  types_diff       = map snd fields_diff
  args_diff        = map fst fields_diff
  arrows           = types_diff <> [base1, base2]
  proper constr name =
    (if name `elem` args_diff
      then ""
      else "o.") <> fieldNameTransform constr name



-- | Converts 1 to 2
--
tplConvertRecord_Haskell :: InteropOptions -> (String, String, [(String, String)]) -> (String, string, [(String, String)]) -> String
tplConvertRecord_Haskell InteropOptions{..} (base1,constr1,fields1) (base2,constr2,fields2) =
     printf "%s :: %s\n" fn_name (tplArrows arrows)
  <> printf "%s %s %s{..} =\n" fn_name (tplArguments args_diff) base1
  <> spaces spacingIndent <> printf "%s {\n" base2
  <> intercalateMap ",\n" (\(n,_) -> spaces (spacingIndent*2) <> printf "%s = %s" (fieldNameTransform base2 n) (fieldNameTransform base1 n)) fields2
  <> "\n"
  <> spaces spacingIndent <> "}\n"
  where
  fn_name          = firstToLower base1 <> "To" <> base2
  fields_union     = union fields1 fields2
  fields_intersect = intersect fields1 fields2
  fields_diff      = fields2 \\ fields1
  types_diff       = map snd fields_diff
  args_diff        = map fst fields_diff
  arrows           = types_diff <> [base1, base2]



-- | tplRows creates row records of the form:
--
-- type RecR = {
--  row1 :: Type1,
--  rowN :: TypeN
-- }
--
tplRows :: InteropOptions -> String ->  String -> [(String, String)] -> String
tplRows InteropOptions{..} suffix base fields =
     printf "type %s%s = {\n" base suffix
  <> intercalateMap ",\n" (\(n,t) -> spaces spacingIndent <> printf "%s :: %s" (fieldNameTransform base n) t) fields
  <> "\n}\n"



tplDataNormal :: InteropOptions -> String -> [(String, [String])] -> String
tplDataNormal InteropOptions{..} base fields =
     printf "data %s\n" base
  <> spaces spacingIndent <> "= "
  <> intercalateMap (spaces spacingIndent <> "| ") (\(n,t) -> printf "%s %s\n" n (intercalate " " t)) fields
  <> "\n"



tplMk :: InteropOptions -> String -> String -> [(String, String)] -> String
tplMk InteropOptions{..} base constr fields =
     printf "mk%s :: " base
  <> intercalateMap " -> " (\(_,t) -> t) (fields <> [("tmp",base)]) <> "\n"
  <> printf "mk%s " base
  <> intercalateMap " " (\(n,_) -> fieldNameTransform base n) fields <> " =\n"
  <> (spaces spacingIndent) <> constr <> "{"
  <> intercalateMap ", " (\(n,_) -> fieldNameTransform base n) fields
  <> "}\n"



-- TODO FIXME: need to also generate a type sig
--
tplUnwrap :: InteropOptions -> String -> String -> [(String, String)] -> String
tplUnwrap InteropOptions{..} base constr fields =
     printf "unwrap%s :: %s -> {\n" base base
  <> intercalateMap ",\n" (\(n,t) -> spaces spacingIndent <> printf "%s :: %s" (fieldNameTransform base n) t) fields
  <> "\n}\n"
  <> printf "unwrap%s (%s r) = r" base constr



tplToJSON_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplToJSON_Record InteropOptions{..} base constr fields =
     instance_decl
  <> spaces si1 <> tojson_decl
  <> spaces si2 <> printf "[ \"tag\" .= \"%s\"\n" base
  <> concatMap (\(n,_) -> spaces si2 <> printf ", \"%s\" .= %s\n" (jsonNameTransform base n) (fieldref_decl n)) fields
  <> spaces si2 <> printf "]\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sToJson :: ToJSON %s where\n" (firstToLower base) base
      LangHaskell    -> printf "instance ToJSON %s where\n" base
  tojson_decl =
    case lang of
      LangPurescript -> printf "toJSON (%s v) = object $\n" constr
      LangHaskell    -> printf "toJSON %s{..} = object $\n" constr
  fieldref_decl n =
    case lang of
      LangPurescript -> printf "v.%s" (fieldNameTransform base n)
      LangHaskell    -> fieldNameTransform base n



tplFromJSON_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplFromJSON_Record InteropOptions{..} base constr fields =
     instance_decl
  <> spaces si1 <> parsejson_decl
  <> concatMap (\(n,_) -> spaces si2 <> printf "%s <- o .: \"%s\"\n" (fieldNameTransform base n) (jsonNameTransform base n)) fields
  <> spaces si2 <> printf "pure $ %s {\n" constr
  <> intercalateMap ",\n" (\(n,_) -> spaces si3 <> printf "%s %s %s" (fieldNameTransform base n)  eql (fieldNameTransform base n)) fields
  <> "\n" <> spaces si2 <> "}\n"
  <> spaces spacingIndent <> printf "parseJSON x = fail $ \"Could not parse object: \" <> show x\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sFromJSON :: FromJSON %s where\n" (firstToLower base) base
      LangHaskell    -> printf "instance FromJSON %s where\n" base
  eql =
    case lang of
      LangPurescript -> ":"
      LangHaskell    -> "="
  parsejson_decl =
    case lang of
      LangPurescript -> "parseJSON (JObject o) = do\n"
      LangHaskell    -> "parseJSON (Object o) = do\n"



tplToJSON_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplToJSON_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> concatMap (\(f,vars) -> tplToJSON_SumType_Field opts f vars) fields
  where
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sToJSON :: ToJSON %s where\n" (firstToLower base) base
      LangHaskell    -> printf "instance ToJSON %s where\n" base



tplToJSON_SumType_Field :: InteropOptions -> String -> [String] -> String
tplToJSON_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "toJSON (%s %s) = object $\n" field (intercalate " " vars')
  <> spaces si2 <> printf "[ \"tag\" .= \"%s\"\n" field
  <>
     (if null vars
        then spaces si2 <> printf ", \"contents\" .= %s\n" (tplArrayString lang)
        else spaces si2 <> printf ", \"contents\" .= " <> wrapContent' vars (intercalateMap ", " ("toJSON " <>) vars') <> "\n")
  <> spaces si2 <> "]\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  vars' = vars_x $ length vars



tplFromJSON_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplFromJSON_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> spaces si1 <> printf "parseJSON (%s o) = do\n" (tplJObject lang)
  <> spaces si2 <> "tag <- o .: \"tag\"\n"
  <> spaces si2 <> "case tag of\n"
  <> concatMap (\(f,vars) -> tplFromJSON_SumType_Field opts f vars) fields
  <> spaces si1 <> printf "parseJSON x = fail $ \"Could not parse object: \" <> show x\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sFromJSON :: FromJSON %s where\n" (firstToLower base) base
      LangHaskell    -> printf "instance FromJSON %s where\n" base



tplFromJSON_SumType_Field :: InteropOptions -> String -> [String] -> String
tplFromJSON_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "\"%s\" -> do\n" field
  <>
     (if null vars
       then spaces si2 <> printf "pure %s\n" field
       else
            spaces si2 <> "r <- o .: \"contents\"\n"
         <> spaces si2 <> "case r of\n"
         <> spaces si3 <> wrapContent' vars (intercalate ", " vars') <> " -> " <> printf "%s <$> %s" field (intercalateMap " <*> " ("parseJSON " <>) vars') <> "\n"
         <> spaces si3 <> printf "_ -> fail \"FromJON Typemismatch: %s\"\n" field)
  <> "\n"
  where
  si1 = spacingIndent*3
  si2 = spacingIndent*4
  si3 = spacingIndent*5
  vars' = vars_x $ length vars




tplEncodeJson_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplEncodeJson_Record InteropOptions{..} base constr fields =
     instance_decl
  <> spaces si1 <> printf "encodeJson (%s o) =\n" constr
  <> spaces si3 <> printf " \"tag\" := \"%s\"\n" base
  <> concatMap (\(n,_) -> spaces si2 <> printf "~> \"%s\" := o.%s\n" (jsonNameTransform base n) (fieldNameTransform base n)) fields
  <> spaces si2 <> "~> jsonEmptyObject\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sEncodeJson :: EncodeJson %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplDecodeJson_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplDecodeJson_Record InteropOptions{..} base constr fields =
     instance_decl
  <> spaces si1 <> "decodeJson o = do\n"
  <> spaces si2 <> "obj <- decodeJson o\n"
  <> concatMap (\(n,_) -> spaces si2 <> printf "%s <- obj .? \"%s\"\n" (fieldNameTransform base n) (jsonNameTransform base n)) fields
  <> spaces si2 <> printf "pure $ %s {\n" constr
  <> intercalateMap ",\n" (\(n,_) -> spaces si3 <> (fieldNameTransform base n)) fields
  <> "\n" <> spaces si2 <> "}\n"
  where
  si1 = spacingIndent*1
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sDecodeJson :: DecodeJson %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplEncodeJson_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplEncodeJson_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> concatMap (\(f,vars) -> tplEncodeJson_SumType_Field opts f vars) fields
  where
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sEncodeJson :: EncodeJson %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplEncodeJson_SumType_Field :: InteropOptions -> String -> [String] -> String
tplEncodeJson_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "encodeJson (%s %s) =\n" field (intercalate " " vars')
  <> spaces si3 <> printf " \"tag\" := \"%s\"\n" field
  <>
     (if null vars
        then spaces si2 <> printf "~> \"contents\" := %s\n" (tplArrayString lang)
        else spaces si2 <> printf "~> \"contents\" := " <> wrapContent' vars (intercalateMap ", " ("encodeJson " <>) vars') <> "\n")
  <> spaces si2 <> "~> jsonEmptyObject\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  vars' = vars_x $ length vars



tplDecodeJson_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplDecodeJson_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> spaces si1 <> "decodeJson json = do\n"
  <> spaces si2 <> "obj <- decodeJson json\n"
  <> spaces si2 <> "tag <- obj .? \"tag\"\n"
  <> spaces si2 <> "case tag of\n"
  <> concatMap (\(f,vars) -> tplDecodeJson_SumType_Field opts f vars) fields
  <> spaces si3 <> printf "_ -> Left $ \"DecodeJson TypeMismatch for %s\"\n\n" base
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sDecodeJson :: DecodeJson %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplDecodeJson_SumType_Field :: InteropOptions -> String -> [String] -> String
tplDecodeJson_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "\"%s\" -> do\n" field
  <>
     (if null vars
       then spaces si2 <> printf "pure %s\n" field
       else
            spaces si2 <> "r <- obj .? \"contents\"\n"
         <> spaces si2 <> "case r of\n"
         <> spaces si3 <> wrapContent' vars (intercalate ", " vars') <> " -> " <> printf "%s <$> %s" field (intercalateMap " <*> " ("decodeJson " <>) vars') <> "\n"
         <> spaces si3 <> printf "_ -> Left $ \"DecodeJson TypeMismatch for %s\"\n\n" field)
  <> "\n"
  where
  si1 = spacingIndent*3
  si2 = spacingIndent*4
  si3 = spacingIndent*5
  vars' = vars_x $ length vars



tplRequestable :: InteropOptions -> String -> String
tplRequestable InteropOptions{..} base =
     printf "instance %sRequestable :: Requestable %s where\n" (firstToLower base) base
  <> spaces si1 <> "toRequest s =\n"
  <> spaces si2 <> "let str = printJson (encodeJson s) :: String\n"
  <> spaces si2 <> "in toRequest str\n"
  where
  si1 = spacingIndent
  si2 = spacingIndent*2



tplRespondable_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplRespondable_Record InteropOptions{..} base constr fields =
     printf "instance %sRespondable :: Respondable %s where\n" (firstToLower base) base
  <> spaces si1 <> "responseType =\n"
  <> spaces si2 <> "Tuple Nothing JSONResponse\n"
  <> spaces si1 <> "fromResponse json =\n"
  <> spaces si3 <> printf "mk%s\n" base
  <> spaces si4 <> "<$> " <> intercalateMap (spaces si4 <> "<*> ") (\(n,t) -> readProp n t) fields
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  si3 = spacingIndent*3
  si4 = spacingIndent*3
  readProp n t =
    if isMaybe t
      then printf "(unNullOrUndefined <$> readProp \"%s\" json)\n" (jsonNameTransform constr n)
      else printf "readProp \"%s\" json\n" (jsonNameTransform constr n)



tplRespondable_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplRespondable_SumType opts@InteropOptions{..} base vars =
     printf "instance %sRespondable :: Respondable %s where\n" (firstToLower base) base
  <> spaces si1 <> "responseType =\n"
  <> spaces si2 <> "Tuple Nothing JSONResponse\n"
  <> spaces si1 <> "fromResponse json = do\n"
  <> spaces si2 <> "tag <- readProp \"tag\" json\n"
  <> spaces si2 <> "case tag of\n"
  <> concatMap (\(f,vars') -> tplRespondable_SumType_Field opts f vars') vars
  <> spaces si3 <> printf "_ -> Left $ TypeMismatch \"%s\" \"Respondable\"\n\n" base
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  si3 = spacingIndent*3



tplRespondable_SumType_Field :: InteropOptions -> String -> [String] -> String
tplRespondable_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "\"%s\" -> do\n" field
  <>
     (if null vars
       then spaces si2 <> printf "pure %s\n" field
       else
            spaces si2 <> "r <- readProp \"contents\" json\n"
         <> spaces si2 <> "case r of\n"
         <> spaces si3 <> wrapContent' vars (intercalate ", " vars') <> " -> " <> printf "%s <$> %s" field (intercalateMap " <*> " ("read " <>) vars') <> "\n"
         <> spaces si3 <> printf "_ -> Left $ TypeMismatch \"%s\" \"Respondable\"\n\n" field)
  <> "\n"
  where
  si1 = spacingIndent*3
  si2 = spacingIndent*4
  si3 = spacingIndent*5
  vars' = vars_x $ length vars



tplIsForeign_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplIsForeign_Record InteropOptions{..} base _ fields =
     printf "instance %sIsForeign :: IsForeign %s where\n" (firstToLower base) base
  <> spaces si1 <> "read json =\n"
  <> spaces si3 <> printf "mk%s\n" base
  <> spaces si4 <> "<$> " <> intercalateMap (spaces si4 <> "<*> ") (\(n,t) -> readProp n t) fields
  where
  si1 = spacingIndent
  si3 = spacingIndent*3
  si4 = spacingIndent*3
  readProp n t =
    if isMaybe t
      then printf "(unNullOrUndefined <$> readProp \"%s\" json)\n" (jsonNameTransform base n)
      else printf "readProp \"%s\" json\n" (jsonNameTransform base n)



tplIsForeign_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplIsForeign_SumType opts@InteropOptions{..} base vars =
     printf "instance %sIsForeign :: IsForeign %s where\n" (firstToLower base) base
  <> spaces si1 <> "read json = do\n"
  <> spaces si2 <> "tag <- readProp \"tag\" json\n"
  <> spaces si2 <> "case tag of\n"
  <> concatMap (\(f,vars') -> tplIsForeign_SumType_Field opts f vars') vars
  <> spaces si3 <> printf "_ -> Left $ TypeMismatch \"%s\" \"IsForeign\"\n\n" base
  where
  si1 = spacingIndent
  si2 = spacingIndent*2
  si3 = spacingIndent*3



tplIsForeign_SumType_Field :: InteropOptions -> String -> [String] -> String
tplIsForeign_SumType_Field InteropOptions{..} field vars =
     spaces si1 <> printf "\"%s\" -> do\n" field
  <>
     (if null vars
       then spaces si2 <> printf "pure %s\n" field
       else
            spaces si2 <> "r <- readProp \"contents\" json\n"
         <> spaces si2 <> "case r of\n"
         <> spaces si3 <> wrapContent' vars (intercalate ", " vars') <> " -> " <>  printf "%s <$> %s" field (intercalateMap " <*> " ("read " <>) vars') <> "\n"
         <> spaces si3 <> printf "_ -> Left $ TypeMismatch \"%s\" \"IsForeign\"\n\n" field)
  <> "\n"
  where
  si1 = spacingIndent*3
  si2 = spacingIndent*4
  si3 = spacingIndent*5
  vars' = vars_x $ length vars



tplShow_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplShow_Record InteropOptions{..} base constr fields =
     instance_decl
  <> intercalateMap " <> \", \" <> " (\(f,_) -> printf "show \"%s: \" <> show o.%s" (fieldNameTransform constr f) (fieldNameTransform constr f)) fields
  where
  si2 = spacingIndent*2
  instance_decl =
    case lang of
      LangPurescript ->
           printf "instance %sShow :: Show %s where\n" (firstToLower base) base
        <> spaces si2 <> printf "show (%s o) = " constr
      LangHaskell    -> haskellNotSupported



tplShow_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplShow_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> concatMap (\(f,vars) -> tplShow_SumType_Field opts f vars) fields
  where
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sShow :: Show %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplShow_SumType_Field :: InteropOptions -> String -> [String] -> String
tplShow_SumType_Field InteropOptions{..} field vars =
 (if null vars
    then
      spaces si1 <> printf "show (%s) = \"%s\"" field field
    else
      spaces si1 <> printf "show (%s %s) = \"%s: \" <> " field (intercalate " " vars') field <> (intercalateMap " <> \" \" <> " (printf "show %s") vars'))
  <> "\n"
  where
  si1 = spacingIndent
  vars' = vars_x $ length vars



tplEq_Record :: InteropOptions -> String -> String -> [(String, String)] -> String
tplEq_Record InteropOptions{..} base constr fields =
     instance_decl
  <> intercalateMap " && " (\(f,_) -> printf "a.%s == b.%s" (fieldNameTransform constr f) (fieldNameTransform constr f)) fields
  where
  si1 = spacingIndent
  instance_decl =
    case lang of
      LangPurescript ->
           printf "instance %sEq :: Eq %s where\n" (firstToLower base) base
        <> spaces si1 <> printf "eq (%s a) (%s b) = " constr constr
      LangHaskell    -> haskellNotSupported



tplEq_SumType :: InteropOptions -> String -> [(String, [String])] -> String
tplEq_SumType opts@InteropOptions{..} base fields =
     instance_decl
  <> concatMap (\(f,vars) -> tplEq_SumType_Field opts f vars) fields
  <> spaces si1 <> "eq _ _ = false"
  where
  si1 = spacingIndent
  instance_decl =
    case lang of
      LangPurescript -> printf "instance %sEq :: Eq %s where\n" (firstToLower base) base
      LangHaskell    -> haskellNotSupported



tplEq_SumType_Field :: InteropOptions -> String -> [String] -> String
tplEq_SumType_Field InteropOptions{..} field vars =
 (if null vars
    then
      spaces si1
      <>
      printf "eq (%s) (%s) = true" field field
    else
      spaces si1
      <>
      printf "eq (%s %s) (%s %s) = %s"
        field (intercalate " " vars'a)
        field (intercalate " " vars'b)
        (intercalateMap (" && " :: String) (\(a,b) -> printf "%s == %s" a b) (zip vars'a vars'b))
  )
  <> "\n"
  where
  si1 = spacingIndent
  vars'  = vars_x $ length vars
  vars'a = map (<>"a") vars'
  vars'b = map (<>"b") vars'



tplImports :: InteropOptions -> String -> String
tplImports InteropOptions{..} =
  case lang of
    LangPurescript -> tplPurescriptImports
    LangHaskell    -> tplHaskellImports



tplPurescriptImports :: String -> String
tplPurescriptImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Control.Monad.Aff                ()"
  , "import Data.Argonaut.Core               (jsonEmptyObject)"
  , "import Data.Argonaut.Decode             (class DecodeJson, decodeJson)"
  , "import Data.Argonaut.Decode.Combinators ((.?))"
  , "import Data.Argonaut.Encode             (class EncodeJson, encodeJson)"
  , "import Data.Argonaut.Encode.Combinators ((~>), (:=))"
  , "import Data.Argonaut.Printer            (printJson)"
  , "import Data.Date.Helpers                (Date(..))"
  , "import Data.Either                      (Either(..))"
  , "import Data.Foreign                     (ForeignError(..))"
  , "import Data.Foreign.NullOrUndefined     (unNullOrUndefined)"
  , "import Data.Foreign.Class               (class IsForeign, read, readProp)"
  , "import Data.List                        (List ())"
  , "import Data.Maybe                       (Maybe(..))"
  , "import Data.Set                         (Set ())"
  , "import Data.Tuple                       (Tuple(..))"
  , "import Network.HTTP.Affjax.Request      (class Requestable, toRequest)"
  , "import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))"
  , "import Optic.Core                       ((^.), (..))"
  , "import Optic.Types                      (Lens, Lens')"
  , "import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==))"
  , ""
  , ""
  ]) <> s



tplHaskellImports :: String -> String
tplHaskellImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Data.Aeson"
  , "import Data.Text   (Text)"
  , "import Data.Monoid ((<>))"
  , ""
  , ""
  ]) <> s



tplApiImports :: InteropOptions -> String -> String
tplApiImports InteropOptions{..} =
  case lang of
    LangPurescript -> tplPurescriptApiImports
    LangHaskell    -> tplHaskellApiImports



tplPurescriptApiImports :: String -> String
tplPurescriptApiImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Purescript.Api.Helpers"
  , "import Prelude"
  , "import Data.Either"
  , "import Data.Tuple"
  , ""
  , ""
  ]) <> s



tplHaskellApiImports :: String -> String
tplHaskellApiImports s = (intercalate "\n"
  [ ""
  , ""
  , "import Haskell.Api.Helpers"
  , "import Data.Text (Text)"
  , "import qualified Data.Text as T"
  , "import Data.Int"
  , ""
  , ""
  ]) <> s



tplHeader :: String -> String -> String
tplHeader header s = header <> "\n" <> s



tplFooter :: String -> String -> String
tplFooter footer s = s <> "\n" <> footer



tplJObject :: Lang -> String
tplJObject LangPurescript = "JObject"
tplJObject LangHaskell    = "Object"



tplArrayString :: Lang -> String
tplArrayString LangPurescript = "([] :: Array String)"
tplArrayString LangHaskell    = "([] :: [Text])"



tplTestHeader :: String -> String
tplTestHeader module_name =
  intercalate "\n" $
  [ "{-# LANGUAGE ExtendedDefaultRules #-}"
  , "{-# LANGUAGE OverloadedStrings    #-}"
  , "{-# LANGUAGE RecordWildCards      #-}"
  , "{-# LANGUAGE ExplicitForAll       #-}"
  , "{-# LANGUAGE RankNTypes           #-}"
  , ""
  , "module " <> module_name <> " where"
  , ""
  , ""
  ]



tplApiEntry :: InteropOptions -> ApiEntry -> String
tplApiEntry opts@InteropOptions{..} api_entry =
  case lang of
    LangPurescript -> tplApiEntry' opts api_entry
    LangHaskell    -> tplApiEntry' opts api_entry



tplApiEntry' :: InteropOptions -> ApiEntry -> String
tplApiEntry' opts@InteropOptions{..} (ApiEntry route params methods) =
  intercalateMap
    "\n"
    (\(param, method) ->
      tplApiEntry'' opts False route param method
      <> "\n" <>
      tplApiEntry'' opts True route param method
    )
    [ (param, method) | param <- params, method <- methods ]



tplApiEntry'' :: InteropOptions -> Bool -> String -> ApiParam -> ApiMethod -> String
tplApiEntry'' opts@InteropOptions{..} simplified route param method =
     printf "%s :: %s%s\n"
       fn_name'
       prolog
       (tplArrows typesig')
  <> printf "%s %s = handleError <$> %s\n"
       fn_name'
       (tplArguments args')
       action
  where
  prolog =
    if simplified
      then ""
      else "forall qp. QueryParam qp => "
  typesig  =
    (tplApiParam_TypeSig opts param) <>
    [tplApiParam_ByType opts param] <>
    [tplApiMethod_RequestType opts method] <>
    [printf "ApiEff (Either ApiError " <> tplApiMethod_ResultType opts method <> ")"]
  typesig' =
    if simplified
      then typesig
      else paramsType' : typesig
  paramsType' =
    case lang of
      LangPurescript -> "Array qp"
      LangHaskell    -> "[qp]"
  args     =
    (tplApiParam_Arguments opts param) <>
    [tplApiParam_ByName opts param] <>
    [jsonNameTransform "" $ tplApiMethod_RequestType opts method]
  args'    =
    if simplified
      then args
      else ["params"] <> args
  method'  = tplApiMethod_Prefix opts method
  byname   = tplApiParam_ByName opts param
  fn_name  = fieldNameTransform "" (method' <> route <> byname)
  fn_name' =
    if simplified
      then fn_name <> "'"
      else fn_name
  action = tplApiAction opts simplified route method param args



tplEmptyQueryParams :: InteropOptions -> String
tplEmptyQueryParams InteropOptions{..} =
  case lang of
    LangPurescript -> "([] :: Array Boolean)"
    LangHaskell    -> "([] :: [(Text, Text)])"



tplApiAction :: InteropOptions -> Bool -> String -> ApiMethod -> ApiParam -> [String] -> String
tplApiAction opts@InteropOptions{..} simplified route api_method api_param args =
  case api_method of
    ApiGET _    -> printf "getAt %s %s" params paths
    ApiPOST _ _ -> printf "postAt %s %s %s" params paths (last args)
    ApiPUT _ _  -> printf "putAt %s %s %s" params paths (last args)
    ApiDELETE _ -> printf "deleteAt %s %s" params paths
  where
  (firstarg, firstarg') =
    if simplified
      then (tplEmptyQueryParams opts, False)
      else ("params", True)
  (by, by') =
    case (tplApiParam_By opts api_param) of
      "" -> ("[]", False)
      v  -> ("[" <> v <> "]", True)
  paths    = "[" <> (intercalate ", " $ ["\"" <> route' <> "\""] <> tplApiParam_Arguments_Show opts api_param) <> "]"
  route'   = jsonNameTransform "" route
  params =
    case (firstarg', by') of
      (False, False) -> tplEmptyQueryParams opts
      (False, _)     -> by
      (_, False)     -> firstarg
      (_, _)         -> (printf "(map qp %s <> map qp %s)" firstarg by) :: String



tplApiMethod_Prefix :: InteropOptions -> ApiMethod -> String
tplApiMethod_Prefix _ (ApiGET _)    = "Get"
tplApiMethod_Prefix _ (ApiPOST _ _) = "Post"
tplApiMethod_Prefix _ (ApiPUT _ _)  = "Put"
tplApiMethod_Prefix _ (ApiDELETE _) = "Delete"



tplApiMethod_ResultType :: InteropOptions -> ApiMethod -> String
tplApiMethod_ResultType opts (ApiGET r)    = tplBuildType opts r
tplApiMethod_ResultType opts (ApiPOST _ r) = tplBuildType opts r
tplApiMethod_ResultType opts (ApiPUT _ r)  = tplBuildType opts r
tplApiMethod_ResultType opts (ApiDELETE r) = tplBuildType opts r



tplApiMethod_RequestType :: InteropOptions -> ApiMethod -> String
tplApiMethod_RequestType _ (ApiGET _)    = ""
tplApiMethod_RequestType opts (ApiPOST r _) = tplBuildType opts r
tplApiMethod_RequestType opts (ApiPUT r _)  = tplBuildType opts r
tplApiMethod_RequestType _ (ApiDELETE _) = ""



tplApiParam_TypeSig :: InteropOptions -> ApiParam -> [String]
tplApiParam_TypeSig opts@InteropOptions{..} param =
  case param of
    Par params       -> map (tplBuildType opts . snd) params
    ParBy _ _        -> []
    ParBoth params _ -> map (tplBuildType opts . snd) params
    ParNone          -> []



tplApiParam_Arguments :: InteropOptions -> ApiParam -> [String]
tplApiParam_Arguments _ param =
  case param of
    Par params       -> map fst params
    ParBy _ _        -> []
    ParBoth params _ -> map fst params
    ParNone          -> []



tplApiParam_Arguments_Show :: InteropOptions -> ApiParam -> [String]
tplApiParam_Arguments_Show InteropOptions{..} param =
  case param of
    Par params       -> map go params
    ParBy _ _        -> []
    ParBoth params _ -> map go params
    ParNone          -> []
  where
  go (n,t) =
    if isString t
      then n
      else
        case lang of
          LangPurescript -> "show " <> n
          LangHaskell    -> "T.pack $ show " <> n



tplApiParam_ByName :: InteropOptions -> ApiParam -> String
tplApiParam_ByName _ param =
  case param of
    Par _              -> ""
    ParBy name _       -> "_" <> name
    ParBoth _ (name,_) -> "_" <> name
    ParNone            -> ""



tplApiParam_ByType :: InteropOptions -> ApiParam -> String
tplApiParam_ByType opts@InteropOptions{..} param =
  case param of
    Par _               -> ""
    ParBy    _ type_    -> tplBuildType opts type_
    ParBoth _ (_,type_) -> tplBuildType opts type_
    ParNone             -> ""



tplApiParam_By :: InteropOptions -> ApiParam -> String
tplApiParam_By opts@InteropOptions{..} param =
  case param of
    Par _              -> ""
    ParBy name _       -> printf "%s %s" name (tplApiParam_ByName opts param)
    ParBoth _ (name,_) -> printf "%s %s" name (tplApiParam_ByName opts param)
    ParNone            -> ""



tplBuildFields :: InteropOptions -> [InternalRep] -> [String]
tplBuildFields opts@InteropOptions{..} ir =
  nub $ sort $ concat $ map go ir
  where
  go (NewtypeRecIR _ constr fields) = map (\(field,_) -> tplBuildField opts constr field) fields
  go (DataRecIR _ constr fields)    = map (\(field,_) -> tplBuildField opts constr field) fields
  go _                              = []



tplBuildField :: InteropOptions -> String -> String -> String
tplBuildField InteropOptions{..} base field =
  case M.lookup name reservedMap of
    Nothing  -> name
    Just new -> new
  where
  name  = fieldNameTransform base field



tplBuildType :: InteropOptions -> String -> String
tplBuildType InteropOptions{..} name =
  case M.lookup name_sanitized typeMap of
    Nothing -> name_sanitized
    Just t  -> t
  where
  name_sanitized = trim name



tplArrows :: [String] -> String
tplArrows = intercalate " -> " . filter (/= "")



tplArguments :: [String] -> String
tplArguments = intercalate " " . filter (/= "")



isMaybe :: String -> Bool
isMaybe s = any (\p -> isPrefixOf p s) ["(Maybe", "DateMaybe"]



isString :: String -> Bool
isString s = any (\p -> isPrefixOf p s) ["String", "Text"]



trim :: String -> String
trim = unwords . words

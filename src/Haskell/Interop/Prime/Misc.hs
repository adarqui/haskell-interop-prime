module Haskell.Interop.Prime.Misc (
  firstToLower,
  toLowerString,
  newlines,
  spaces,
  vars_x,
  wrapContent,
  wrapContent',
  intercalateMap,
  haskellNotSupported,
  purescriptNotSupported
) where



import           Data.Char   (toLower)
import           Data.List   (intercalate)
import           Data.Monoid ((<>))



firstToLower :: String -> String
firstToLower [] = []
firstToLower (x:xs) = toLower x:xs



toLowerString :: String -> String
toLowerString = map toLower



newlines :: Int -> String
newlines = flip replicate '\n'


spaces :: Int -> String
spaces = flip replicate ' '



vars_x :: Int -> [String]
vars_x n = map (("x" <>) . show) [0..n - 1]



wrapContent :: [a] -> String -> String
wrapContent vars str | length vars == 1 = str
                     | otherwise        = "[" <> str <> "]"



wrapContent' :: [a] -> String -> String
wrapContent' _ str = "[" <> str <> "]"



intercalateMap :: [b] -> (a -> [b]) -> [a] -> [b]
intercalateMap t f xs = intercalate t $ map f xs



haskellNotSupported :: String
haskellNotSupported = error "Haskell not supported."



purescriptNotSupported :: String
purescriptNotSupported = error "Purescript not supported."

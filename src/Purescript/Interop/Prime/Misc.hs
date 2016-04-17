module Purescript.Interop.Prime.Misc (
  firstToLower,
  newlines,
  spaces,
  vars_x,
  wrapContent,
  haskellNotSupported,
  purescriptNotSupported
) where



import           Data.Char



firstToLower :: String -> String
firstToLower [] = []
firstToLower (x:xs) = toLower x:xs



newlines :: Int -> String
newlines = flip replicate '\n'


spaces :: Int -> String
spaces = flip replicate ' '



vars_x :: Int -> [String]
vars_x n = map (("x" ++) . show) [0..n - 1]



wrapContent :: [a] -> String -> String
wrapContent vars str | length vars == 1 = str
                     | otherwise        = "[" ++ str ++ "]"



haskellNotSupported :: String
haskellNotSupported = error "Haskell not supported."



purescriptNotSupported :: String
purescriptNotSupported = error "Purescript not supported."

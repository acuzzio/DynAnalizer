module Functions where

import Data.List.Split
import Data.List
import Text.Printf

read2 :: String -> Double
read2 x = read x :: Double

writeF :: [[[String]]] -> String
writeF x  = intercalate "  \n"$ map unlines $ map (map unwords) x

printZ x    = (printf "%.2f" x) :: String

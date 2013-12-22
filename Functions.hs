module Functions where
import Data.List.Split
import Data.List

read2 :: String -> Double
read2 x = read x :: Double

writeF :: [[[String]]] -> String
writeF x  = intercalate "  \n"$ map unlines $ map (map unwords) x

module Functions where

import Control.Concurrent.Async
import Data.List.Split
import Data.List
import Text.Printf
import System.Process

import DataTypes

read2 :: String -> Double
read2 x = read x :: Double

fromIntegral2 :: Int -> Double
fromIntegral2 x = fromIntegral x :: Double

writeF :: [[[String]]] -> String
writeF x  = intercalate "  \n"$ map unlines $ map (map unwords) x

avgListaListe :: [[Double]] -> [Double]
avgListaListe xss = map avg $ transpose xss

avg xs   = (sum xs)/(fromIntegral $ length xs)

devSt :: [Double] -> Double
devSt xs = let
  average = avg xs
  differences = map (\x -> x - average) xs
  l       = fromIntegral (length xs )
  in sqrt $ (sum(map (\x-> x**2) (differences)))/l

printZ x    = (printf "%.2f" x) :: String

printZ12 x    = (printf "%.10f" x) :: String

compress :: Eq a => [a] -> [a] 
compress = map head . group

findInd :: Plottable -> [Plottable] -> Int
findInd plo plos = let Just x = elemIndex plo plos
                     in x + 2

-- parallel attempt

parallelProcFiles :: (a -> IO b) -> [a] -> IO ()
parallelProcFiles function outputs = do
       pids <- mapM (\x -> async $ function x) outputs
       mapM_ wait pids


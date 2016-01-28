module Functions where

import Control.Concurrent.Async
import Data.List.Split
import Data.List
import Text.Printf
import System.Directory
import System.Process

import DataTypes

read2 :: String -> Double
read2 x = read x :: Double

readI :: String -> Int
readI x = read x :: Int

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

correctFolderName :: String -> IO String
correctFolderName fn = if fn == "." 
   then do
        a  <- getCurrentDirectory
        let aa  = if (last a == '/') then init a else a     -- when the folder has '/' at the end
            -- aaa = reverse $ takeWhile (/= '/') $ reverse aa -- this was the bug of using Dynanalizer with the '.' from inside the project folder.
        return aa
   else do
        let aa  = if (last fn == '/') then init fn else fn
        return aa

mosaic = map words . lines

unmosaic = unlines . map unwords

convFStoAU = 41.3414472
convAUtoFS =  0.0241888



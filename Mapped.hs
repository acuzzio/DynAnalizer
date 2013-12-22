module Mapped where

import Data.List
import System.Environment

import Functions

-- filename="CASPT2ccccTotal"
resolution=20
-- cia=[["99","207","-199.36577"],["99","208","-199.07044"],["99","209","-198.8451"],["99","210","-198.74154"],["99","211","-198.79493"],["99","212","-199.01847"],["99","213","-199.4156"],["99","214","-199.95921"],["99","215","-200.59431"],["99","216","-201.25063"],["99","217","-201.83574"],["99","218","-202.3111"],["99","219","-202.6495"],["99","220","-202.84185"],["99","221","-202.8568"],["99","222","-202.65018"],["99","223","-202.18189"],["99","224","-201.3892"],["99","225","-200.20376"],["99","226","-198.57572"],["99","227","-196.55549"]]

mapped stringZ index name = do
       let just3     = concat $ map (map (\x -> [x!!0,x!!1,x!!index])) stringZ
           floats    = map (map (read2)) just3
           first     = map head floats 
           second    = concat $ map tail floats
           maxfirst  = maximum first
           minfirst  = minimum first
           maxsecon  = maximum second
           minsecon  = minimum second
--           maxfirst = 227.0
--           maxsecon = 108.22675
--           minfirst = 1.0
--           minsecon = -247.94655
--           maxfirst = 227.0
--           minfirst = 1.0
--           maxsecon = 0.4
--           minsecon = -0.4
           ord      = listar maxfirst minfirst resolution
           asc      = listar maxsecon minsecon resolution
           noun     = [(x,y) | x <- (tuplar ord), y <- (tuplar asc)]
           result   = map (\x-> countMe x floats) noun
           result'  = zipWith prettyprint noun result
           result'' = unlines $ addSpace result'
       writeFile name result''

addSpace :: [String] -> [String]
addSpace (x:y:[])= x:y:[]
addSpace (x:y:xs)= if (head $ words x) == (head $ words y) then (x: addSpace (y:xs)) else (x:[]: (addSpace (y:xs)))

prettyprint :: ((Double,Double),(Double,Double)) -> Int -> String
prettyprint ((minS,maxS),(minD,maxD)) a = let
              uno=(minS+maxS)/2.0
              due=(minD+maxD)/2.0
              in show uno ++ " " ++ show due ++ " " ++ show a

listar :: Double -> Double -> Int -> [Double]
listar max min res = let
                    deno = fromIntegral res 
                    step  = (max-min)/(deno-1.0)
                    in take (res) $ iterate (+step) min

tuplar :: [Double] -> [(Double,Double)]
tuplar (x:[]) = []
tuplar (x:xs) = (x,head xs) : tuplar xs

element :: (Double, Double) -> [Double] -> [Int]
element (min,max) [] = []
element (min,max) (x:xs) = if x > min && x < max 
                       then 1 : element (min,max) xs
                       else 0 : element (min,max) xs

howMany :: (Double, Double) -> [Double] -> Int
howMany (min,max) [] = 0
howMany (min,max) xs = sum $ element (min,max) xs

countMeOnElements :: ((Double,Double),(Double,Double)) -> [Double] -> Int
countMeOnElements ((minS,maxS),(minD,maxD)) xs = if (head xs) > minS && (head xs) < maxS && (xs !! 1) > minD && (xs !! 1) < maxD then 1 else 0

countMe :: ((Double,Double),(Double,Double)) -> [[Double]] -> Int
countMe tupla xs = sum $ map (countMeOnElements tupla) xs



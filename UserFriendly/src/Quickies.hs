module Quickies where

import CalculateData
import CreateInfo
import DataTypes
import Filters
import Functions
import ParseInput

import Data.List
import System.ShQQ

-- Molcas Dynamix in pratica fa cosi' lancia il primo step con la prima velocita' e poi lancia il primo tully, ma alla prima chiamata praticamente non ha i coefficienti CI e non genera niente. Ecco perche' le energie ci sono solo alla terza geometria.

-- THIS FUNCTION WORS WITH JUST 2 ROOTS

getEnerFromInfo input (infoName,dataName) = do
   info             <- rdInfoFile infoName
   dataZ            <- readFile dataName
   let dataZZ       = map words $ lines dataZ
       enepop       = map (map show) $ getEnergies info
       energies     = transpose [enepop!!2, enepop!!3]
       energiesDiff = map (\[x,y] -> [x,y, show ((read x :: Double) - (read y :: Double))]) energies
       togheter     = zipWith (++) dataZZ ([]:[]:energiesDiff)
       filtr        = last $ filter (\x -> x !! 11 == "10") togheter
   return filtr

-- input <- getInputInfos "input"

luismaAsk input = do
   atd   <- readerData
   let (doHop,doesNotHop)   = whoHop input atd
       (doLeft,doRight)     = whoLeftWhoRight input doHop
   result <- mapM (getAveragesFromData input) [doLeft,doRight]
   putStrLn $ unlines $ map (unwords . (map show)) result 


getAveragesFromData input a = do
   let label       = map (\x -> (head x) !! 0) a
   tupla           <- mapM getFileName label
   allHops         <- mapM (getEnerFromInfo input) tupla
   let transposT   = transpose allHops
       rightLabel  = [3,5,6,7,8,12,13,14]
       rightValues = map (\x -> transposT !! x) rightLabel
       averages    = map (avg . (map read2)) $ rightValues
   return averages

getFileName x = do
  a <- readShell $ "ls INFO/*" ++ x ++ "*"
  b <- readShell $ "ls DATA/*" ++ x ++ "*"
  return (head (lines a), head (lines b))

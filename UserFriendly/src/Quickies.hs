module Quickies where

import CalculateData
import CreateInfo
import DataTypes
import Functions
import ParseInput

import Data.List

-- Molcas Dynamix in pratica fa cosi' lancia il primo step con la prima velocita' e poi lancia il primo tully, ma alla prima chiamata praticamente non ha i coefficienti CI e non genera niente. Ecco perche' le energie ci sono solo alla terza geometria.


-- funziona solo con due root
getEnerFromInfo input tNum = do
   let infoName     = "INFO/traj" ++ tNum ++ ".info"
       dataName     = "DATA/traj" ++ tNum ++ ".data"
   info             <- rdInfoFile infoName
   dataZ            <- readFile dataName
   let dataZZ       = map words $ lines dataZ
       enepop       = map (map show) $ getEnergies info
       energies     = transpose [enepop!!2, enepop!!3]
       togheter     = zipWith (++) dataZZ ([]:[]:energies)
       filtr        = last $ filter (\x -> x !! 11 == "10") togheter
   return filtr

-- input <- getInputInfos "input"

luismaAsk input = do
   atd   <- readerData
   let (doHop,doesNotHop)   = whoHop input atd
       

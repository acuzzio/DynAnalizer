module Filters where

import Data.List
import System.Directory
import System.Process
import System.ShQQ

import CalculateData
import DataTypes
import Functions
import GnuplotZ
import ParseInput

readerData :: IO AllTrajData
readerData = do
    outs            <- readShell $ "ls DATA/*.data"
    let outputs     = lines outs
    dataContent     <- mapM readFile outputs
    return $ map (map words) $ map lines dataContent

mainfilter input = do
    atd   <- readerData
    let plottable  = getListToPlot input
        checkPlots = map (\x -> x `elemIndex` plottable) [CcccCorrected, Ct, Jump]
        thereSNoth = Nothing `elem` checkPlots
    case thereSNoth of 
      True  -> do putStrLn $ "You cannot use filters without Cccccorrected, Ct and Jump constructors in dataPlot variable"
      False -> do 
          let (doHop,doesNotHop)   = whoHop input atd
              (doIsom,doesNotIsom) = whoIsom input atd
              allOfThem     = (atd, "all")
              doHopIsom     = (intersect doHop doIsom, "HopAndIsom")
              doHopNoIsom   = (intersect doHop doesNotIsom, "HopAndNoIsom")
              noHopIsom     = (intersect doesNotHop doIsom, "NoHopAndIsom")
              noHopNoIsom   = (intersect doesNotHop doesNotIsom, "NoHopNoIsom")
          mapM_ (\x -> buaaaah input (snd x) (fst x)) [allOfThem, doHopIsom,doHopNoIsom,noHopIsom,noHopNoIsom]

buaaaah input lab atd = do
    let trajNum x   = map (\x -> x!!0!!0) x
        folder      = getfolder input
        label       = folder ++ lab 
        nRoot       = getnRoot input
        nRootI      = pred nRoot
        allJumps    = [(show x) ++ (show y) | x <- [0.. nRootI], y <- [0.. nRootI], x/=y]
        plottable   = getListToPlot input
        rightIndex  = findInd Jump plottable
        getHOP root = filter (\x -> x /= []) $ map (filter (\x-> x!!rightIndex == root)) atd
        getHOPs     = map getHOP allJumps
    case length atd of
       0 -> do putStrLn $ "No trajectories are " ++ lab
       otherwise -> do
          writeFile label $ unlines $ map unlines $ map (map unwords) atd
          mapM_ (\x -> writeFile (label ++ fst x) $ writeF (getHOPs !! snd x)) $ zip allJumps [0..]
          mapM (\x -> gnuplotG input lab x atd) [CcccCorrected,BetaCorrected,Tau]
          createDirectoryIfMissing True "Graphics"
          system $ "mv " ++ label ++ "* Graphics"
          print label
          print $ trajNum atd
          print $ length $ trajNum atd
               
extractJust :: Maybe Int -> Int
extractJust a = case a of
   Just x  -> x
   Nothing -> 0

whoIsom :: Inputs -> AllTrajData -> (AllTrajData,AllTrajData)
whoIsom input atd = partition ( isoOrNot input ) atd

isoOrNot :: Inputs -> SingleTrajData -> Bool
isoOrNot input std = let
   listPlot  = getListToPlot input 
   index     = findInd CcccCorrected listPlot
   lastPoint = last std
   lastValue = read2 $ lastPoint !! index
   isomCond  = snd $ getUpperAndIsomCond $ getisomType input 
   in isomCond lastValue

whoHop :: Inputs -> AllTrajData -> (AllTrajData,AllTrajData)
whoHop input atd = let
   listPlot      = getListToPlot input
   indeX         = findInd Jump listPlot
   in partition ( doThisHopOrNot indeX ) atd

doThisHopOrNot :: Int -> SingleTrajData -> Bool
doThisHopOrNot index std = let
   jumpColumn = map (\x-> x!!index) std  
   in any (\x -> x == "10") jumpColumn

clockWiseCounterClockWise :: AllTrajData -> AllTrajData
clockWiseCounterClockWise atd = undefined

filterHoppingPointsAll :: AllTrajData -> AllTrajData
filterHoppingPointsAll atd = map filterHoppingPoints atd

filterHoppingPoints :: SingleTrajData -> SingleTrajData
filterHoppingPoints std = undefined

filterCTHigherOrLowerAll :: Double -> AllTrajData -> AllTrajData
filterCTHigherOrLowerAll thresh atd = map (filterCTHigherOrLower thresh) atd

filterCTHigherOrLower :: Double -> SingleTrajData -> SingleTrajData
filterCTHigherOrLower thresh std = undefined 


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

mainfilter input = do
    atd   <- readerData
    let plottable  = getListToPlot input
        folder     = getfolder input
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
              listOfThem    = [allOfThem, doHopIsom,doHopNoIsom,noHopIsom,noHopNoIsom]
              fileN         = folder ++ "-Stats"
          system $ "rm " ++ fileN ++ " 2> /dev/null"
          mapM_ (\x -> buaaaah input (snd x) (fst x)) listOfThem
          mapM_ (\x -> atdLogger fileN (snd x) (fst x)) listOfThem
          let lf = length . fst
              [all,yhyi,yhni,nhyi,nhni] = map lf listOfThem
              z         = "TOTAL         -> " ++ (show all)
              a         = "Hop and Iso   -> " ++ (show yhyi)
              b         = "Hop not Iso   -> " ++ (show yhni)
              c         = "NoHop and Iso -> " ++ (show nhyi)
              d         = "NoHop not Iso -> " ++ (show nhni)
              e         = "\nonly Hopped Iso/notIso -> " ++ (rateH yhyi yhni) ++ "%"
              f         = "only NON Hopped Iso/notIso -> " ++ (rateH nhyi nhni) ++ "%"
              g         = "Total Iso/notIso -> " ++ (rateH (yhyi+nhyi) (yhni+nhni)) ++ "%"
              stringToW = intercalate "\n" [z,a,b,c,d,e,f,g] 
          putStrLn stringToW
          appendFile fileN stringToW 
          putStrLn $ "\nEverything written down into file: " ++ fileN ++ " !!\n\n"

rateH :: Int -> Int -> String
rateH a b = printZ ((fromIntegral (a * 100) / (fromIntegral (a+b))) :: Double) 


atdLogger filN lab atd = do
          let trajNum x   = map (\x -> x!!0!!0) x
          appendFile filN $ "\n" ++ lab ++ " " 
          appendFile filN $ show $ length $ trajNum atd
          appendFile filN $ ":\n" ++ (unwords $ trajNum atd)
          appendFile filN "\n"

buaaaah input lab atd = do
    let folder      = getfolder input
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
          return ()
               
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

-- Charge Transfer part

chargeTmap :: Inputs -> IO()
chargeTmap input = do
  atd   <- readerData
  let plottable  = getListToPlot input
      folder     = getfolder input
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
          listOfThem    = [allOfThem, doHopIsom,doHopNoIsom,noHopIsom,noHopNoIsom]
          listOfNonEmpt = filter (\x -> (length $ fst x) > 0 ) listOfThem
          thresh = getchargeTrThresh input
      mapM_ (\x -> chargeTMultiple input (fst x) (snd x) thresh) listOfNonEmpt 

chargeTMultiple :: Inputs -> AllTrajData -> String -> [Double] -> IO()
chargeTMultiple input atd filtername thresh = mapM_ (\x -> chargeTsingle input atd filtername x ) thresh

chargeTsingle :: Inputs -> AllTrajData -> String -> Double -> IO()
chargeTsingle input atd filtername thresh = do
    let folder      = getfolder input
        plottable   = getListToPlot input
        rightIndex  = findInd Ct plottable
        nRootI      = pred $ getnRoot input
        allJumps    = [(show x) ++ (show y) | x <- [0.. nRootI], y <- [0.. nRootI], x/=y]
        rightIndHop = findInd Jump plottable
        getHOP root = filter (\x -> x /= []) $ map (filter (\x-> x!!rightIndHop == root)) atd
        getHOPs     = map getHOP allJumps
        upper       = map (filter (\x -> read2 (x!!rightIndex) > thresh)) atd
        lower       = map (filter (\x -> read2 (x!!rightIndex) < thresh)) atd
        upperCorr   = map compress $ zipWith correctGaps upper atd
        lowerCorr   = map compress $ zipWith correctGaps lower atd
        fileName    = "chargeTr" ++ folder ++ (show thresh) ++ filtername 
    mapM_ (\x -> writeFile (fileName ++ fst x) $ writeF (getHOPs !! snd x)) $ zip allJumps [0..]
    writeFile (fileName ++ "HI") $ writeF upperCorr
    writeFile (fileName ++ "LO") $ writeF lowerCorr
    mapM (\x -> gnuplotCT input filtername x atd thresh) [CcccCorrected,BetaCorrected,Tau]
    createDirectoryIfMissing True "GraphicsCT"
    system $ "mv " ++ fileName ++ "* GraphicsCT"
    return ()


-- I wanna fill the space between two different set in gnuplot splot lines
correctGaps :: [[String]] -> [[String]] -> [[String]]
correctGaps []    a      = []
correctGaps small (x:[]) = x : []
correctGaps small (x:xs) = if elem (head xs) small then x : correctGaps small xs else if elem x small then x : correctGaps small xs else [" "] : correctGaps small xs




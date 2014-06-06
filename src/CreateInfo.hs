module CreateInfo where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Data.List.Split
import Data.List
import System.ShQQ
import Text.Printf

import DataTypes
import Functions
import IntCoor

-- Creates info files from molcas output
createInfoQM path = do
       outs <- readShell $ "ls " ++ path
       let outputs = lines outs
       mapM_ genInfoFileQM outputs

---- Creates info files from molcas output
--createInfoQMMM path = do
--       outs <- readShell $ "ls " ++ path
--       let outputs = lines outs
--       mapM_ genInfoFileQMMM outputs

-- Creates info files from molcas output
createInfoQMMM path = do
       outs <- readShell $ "ls " ++ path
       let outputs  = lines outs
           chunks   = chunksOf 10 outputs
       sequence_ $ fmap (processFiles genInfoFileQMMM) chunks

rdInfoFiles :: [FilePath] -> IO([Dinamica])
rdInfoFiles fns = do
    mapM rdInfoFile fns

rdInfoFile  :: FilePath -> IO(Dinamica)
rdInfoFile fn = do
    cont <- readFile fn
    let (aN:rNS:rlxS:dTS:aT:ene:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
        atomN  = read (head aN)   :: Int
        rN     = read (head rNS)  :: Int
        rlx    = read (head rlxS) :: Int
        dT     = read (head dTS)  :: Double
        enepop = splitWhen (== "SUBDIVISION") ene 
        eneflo = map (map (\x -> read x :: Double)) enepop
        coord1 = parseTriplet $ unlines f
        oscStr = map (\x-> read x :: Double) g
        charT  = map (\x-> read x :: Double) h
    return $ Dinamica fn atomN rN rlx dT aT eneflo coord1 oscStr charT

-- to cut an infofile at "step" step, and make it smaller
cutInfoFile :: FilePath -> Int -> IO ()
cutInfoFile fn steps = do
  cont <- readFile fn
  let (aN:rNS:rlxS:dTS:aT:ene:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
      atomN        = read (head aN)   :: Int
      enepop       = splitWhen (== "SUBDIVISION") ene
      newEnepop    = map (take steps) enepop
      newCoord     = concat $ take steps $ chunksOf atomN f
      newOscStr    = concat $ take steps $ chunksOf atomN g
      newMullChar  = concat $ take steps $ chunksOf atomN h
      div          = "DIVISION"
      subDiv       = "SUBDIVISION"
      energiesPop' = intercalate [subDiv] newEnepop
      wholefile    = unlines $ intercalate [div] [aN,rNS,rlxS,dTS,aT,energiesPop',newCoord,newOscStr,newMullChar]
  writeFile (fn ++ "CUT") wholefile 

checkInfoFiles :: FilePath -> IO()
checkInfoFiles path = do
  outs <- readShell $ "ls " ++ path
  let outputs = lines outs
  putStrLn "A Good one of 100 step should be like [98,98,98,98,98,100,99,100]"
  putStrLn "Tully's energies/populations are STEP-2, then we have STEP geometries, STEP-1 Oscillator strength and STEP charge transfers (one for each geometry)"
  mapM_ checkInfoFile outputs

checkInfoFile :: FilePath -> IO()
checkInfoFile fn = do
  cont <- readFile fn
  let (aN:rNS:rlxS:dTS:aT:ene:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
      atomN        = read (head aN)   :: Int
      enepop       = splitWhen (== "SUBDIVISION") ene
      leng1        = map length enepop
      leng2        = length $ chunksOf atomN f
      leng3        = length g
      leng4        = length $ chunksOf atomN h
      fstCheck     = map (\x-> head leng1 == x) leng1 -- all energies population equal?
      sndCheck     = (head leng1) + 2 == leng2 -- STEP geometries
      trdCheck     = (head leng1) + 1 == leng3 -- STEP-1 Oscillator strength
      fthCheck     = (head leng1) + 2 == leng4 -- STEP charge transfers
      allChecks    = and $ fstCheck ++ [sndCheck,trdCheck,fthCheck]
      resultMsg    = case allChecks of
                       True  -> " ->  That's OK !"
                       False -> " ->  This one has problems !"
  putStrLn $ fn ++ " " ++ (show (leng1 ++ [leng2] ++ [leng3] ++ [leng4])) ++ " " ++ resultMsg

genInfoFileQM :: String -> IO ()
genInfoFileQM fn = do
    atomNS                  <- readShell $ "head -500 " ++ fn ++ " | grep -B3 'InterNuclear Distances' | head -1 | awk '{print $1}'"
    rootNS                  <- readShell $ "head -200 " ++ fn ++ " | grep -A1 -i ciro | tail -1 | awk '{print $1}'"
    rlxRtS                  <- readShell $ "head -200 " ++ fn ++ " | grep -A1 -i mdrl | tail -1 | awk '{print $1}'" 
    dTS                     <- readShell $ "head -200 " ++ fn ++ " | grep -i -A1 dt | tail -1 | awk '{print $1}'" 
    let atomNumber          = read atomNS :: Int
        rootN               = read rootNS :: Int
        rlxRtN              = read rlxRtS :: Int
        dT                  = read dTS    :: Double
        grepLength          = show $ atomNumber + 3
        numberFields        = (rootN * 2) + 1
    atomTS                  <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | tail -" ++ (show atomNumber) ++ " | awk '{print $2}' | head -" ++ (show atomNumber)
    energiesPop             <- mapM (\a -> readShell $ "grep OOLgnuplt " ++ fn ++ " | awk '{print $" ++ (show a) ++ "}'") $ map succ [1..numberFields] -- map succ because the first field is the string gnuplot
    coordinates             <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
    oscStr                  <- readShell $ "grep -A2 'Osc. strength.' " ++ fn ++ " | awk 'NR % 4 == 3' | awk '{print $3}'"
    chargeTr                <- readShell $ "awk '/Mulliken population Analysis for root number: 1/ {flag=1;next} /Expectation values of various properties for root number:  1/ {flag=0} flag {print}' " ++ fn ++ " | grep N-E | sed s/N-E//" 
    let infoname            = (reverse (dropWhile (/= '.') $ reverse fn )) ++ "info"
        div                 = "DIVISION\n"
        subDiv              = "SUBDIVISION\n"
        atomTS'             = unlines $ map (\x -> head x :[]) $ lines atomTS 
        energiesPop'        = concat $ intersperse subDiv energiesPop
        chargeTr'           = unlines $ concat $ fmap words $ lines chargeTr
        wholefile           = atomNS ++ div ++ rootNS ++ div ++ rlxRtS ++ div ++ dTS ++ div ++ atomTS' ++ div ++ energiesPop' ++ div ++ coordinates ++ div ++ oscStr ++ div ++ chargeTr'
    writeFile infoname wholefile
    putStrLn $ fn ++ " done"

genInfoFileQMMM :: String -> IO ()
genInfoFileQMMM fn = do
    let infoname            = (reverse (dropWhile (/= '.') $ reverse fn )) ++ "info"
        keyname             = (reverse (dropWhile (/= '.') $ reverse fn )) ++ "key"
    atomNS                  <- readShell $ "grep -B2 'Nuclear repulsion energy' " ++ fn ++ " | head -1 | awk '{print $1}'"
    rootNS                  <- readShell $ "head -200 " ++ fn ++ " | grep -A1 -i ciro | tail -1 | awk '{print $1}'"
    rlxRtS                  <- readShell $ "head -200 " ++ fn ++ " | grep -A1 -i mdrl | tail -1 | awk '{print $1}'" 
    dTS                     <- readShell $ "head -200 " ++ fn ++ " | grep -i -A1 dt | tail -1 | awk '{print $1}'" 
    let atomNumber          = read atomNS :: Int
        rootN               = read rootNS :: Int
        rlxRtN              = read rlxRtS :: Int
        dT                  = read dTS    :: Double
        grepLength          = show $ atomNumber + 3
        numberFields        = (rootN * 2) + 1
    atomTS                  <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | tail -" ++ (show atomNumber) ++ " | awk '{print $2}' | head -" ++ (show atomNumber)
    energiesPop             <- mapM (\a -> readShell $ "grep OOLgnuplt " ++ fn ++ " | awk '{print $" ++ (show a) ++ "}'") $ map succ [1..numberFields] -- map succ because the first field is the string "OOLgnuplt"
    coordinates             <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
    oscStr                  <- readShell $ "grep -A2 'Osc. strength.' " ++ fn ++ " | awk 'NR % 4 == 3' | awk '{print $3}'"
    chargeTr                <- readShell $ "awk '/Mulliken population Analysis for root number: 1/ {flag=1;next} /Expectation values of various properties for root number:  1/ {flag=0} flag {print}' " ++ fn ++ " | grep N-E | sed s/N-E//" 
    let div                 = "DIVISION\n"
        subDiv              = "SUBDIVISION\n"
        atomTS'             = unlines $ map (\x -> head x :[]) $ lines atomTS 
        energiesPop'        = concat $ intersperse subDiv energiesPop
        chargeTr'           = unlines $ concat $ fmap words $ lines chargeTr
        wholefile           = atomNS ++ div ++ rootNS ++ div ++ rlxRtS ++ div ++ dTS ++ div ++ atomTS' ++ div ++ energiesPop' ++ div ++ coordinates ++ div ++ oscStr ++ div ++ chargeTr'
    writeFile infoname wholefile
    putStrLn $ fn ++ " done"


parseTriplet :: String -> [Vec Double]
parseTriplet = fmap (Vec .fmap (readDouble) . words) . lines
     where readDouble = \x -> 0.529177249 * (read x :: Double)


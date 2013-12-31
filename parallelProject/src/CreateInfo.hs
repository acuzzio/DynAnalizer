module CreateInfo where

import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import Control.Monad
import Control.Concurrent.Async
import System.Process

import IntCoor

  
data Dinamica = Dinamica {
          getOutputNam   :: String,
          getAtomN       :: Int,
          getRootN       :: Int,
          getStartRlxRt  :: Int,
          getDT          :: Double,
          getAtomT       :: [String],
          getEnergies    :: [[Double]],
          getCoordinates :: [Vec Double],
          getOscStr      :: [Double],
          getCharTran    :: [Double]
          } deriving Show

createInfoP = do
       outs <- readShell "ls */*.out"
       pwd  <- readShell "pwd"
       let outputs  = lines outs
           foldName = reverse $ takeWhile (\x-> x/='/') $ reverse $ head $ lines pwd -- dull again, but works like a charm
           chunks   = chunksOf 10 outputs
       sequence_ $ processFiles `fmap` chunks
       writeFile "shellforTakeATar.sh" $ shellZ foldName
       

shellZ name = "mkdir temptemp\ncp */*.info temptemp/\ncd temptemp/\ntar -zcvf " ++ name ++ ".tgz *\nmv " ++ name ++ ".tgz ../"

processFiles :: [FilePath] -> IO ()
processFiles outputs = do
       pids <- mapM (\x -> async $ genInfoFile x) outputs
       mapM_ wait pids

rdInfoFile  :: FilePath -> IO(Dinamica)
rdInfoFile fn = do
    cont <- readFile fn
    let (aN:rNS:rlxS:dTS:aT:ene:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
        atomN  = read (head aN) :: Int
        rN     = read (head rNS) :: Int
        rlx    = read (head rlxS) :: Int
        dT     = read (head dTS)  :: Double
        enepop = splitWhen (== "SUBDIVISION") ene 
        eneflo = map (map (\x -> read x :: Double)) enepop
        coord1 = parseTriplet $ unlines f
        oscStr = map (\x-> read x :: Double) g
        charT  = map (\x-> read x :: Double) h
    return $ Dinamica fn atomN rN rlx dT aT eneflo coord1 oscStr charT

genInfoFile :: String -> IO ()
genInfoFile fn = do
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
    atomTS                  <- readShell $ "head -500 " ++ fn ++ " | grep -A" ++ grepLength ++ " ' Cartesian Coordinates' " ++ fn ++ " | tail -" ++ (show atomNumber) ++ " | awk '{print $2}'"
    energiesPop             <- mapM (\a -> readShell $ "grep OOLgnuplt " ++ fn ++ " | awk '{print $" ++ (show a) ++ "}'") $ map succ [1..numberFields] -- map succ because the first field is the sring gnuplot
    coordinates             <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
    oscStr                  <- readShell $ "grep -A2 'Osc. strength.' " ++ fn ++ " | awk 'NR % 4 == 3' | awk '{print $3}'"
    chargeTr                <- readShell $ "awk '/Mulliken population Analysis for root number: 1/ {flag=1;next} /Expectation values of various properties for root number:  1/ {flag=0} flag {print}' " ++ fn ++ " | grep N-E | sed s/N-E//" 
    let infoname            = (takeWhile (/= '.') fn ) ++ ".info"
        div                 = "DIVISION\n"
        subDiv              = "SUBDIVISION\n"
        atomTS'             = unlines $ map (\x -> head x :[]) $ lines atomTS 
        energiesPop'        = concat $ intersperse subDiv energiesPop
        chargeTr'           = unlines $ concat $ fmap words $ lines chargeTr        
        wholefile           = atomNS ++ div ++ rootNS ++ div ++ rlxRtS ++ div ++ dTS ++ div ++ atomTS' ++ div ++ energiesPop' ++ div ++ coordinates ++ div ++ oscStr ++ div ++ chargeTr'
    writeFile infoname wholefile

parseTriplet :: String -> [Vec Double]
parseTriplet = fmap (Vec .fmap (readDouble) . words) . lines
     where readDouble = \x -> 0.529177249 * (read x :: Double)



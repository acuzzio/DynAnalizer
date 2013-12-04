module CreateInfo where

import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import Control.Monad
import Control.Concurrent.Async

import IntCoor
import Inputs



data Dinamica = Dinamica {
          getOutputNam   :: String,
          getAtomN       :: Int,
          getAtomT       :: [String],
          getEnergies    :: ([Double],[Double],[Double],[Double],[Double]),
          getCoordinates :: [Vec Double],
          getOscStr      :: [Double],
          getCharTran    :: [Double]
          } deriving Show

-- Creates info files from molcas output
createInfo = do
       outs <- readShell $ "ls " ++ folder ++ "/*.out"
       let outputs = lines outs
       mapM_ (genInfoFile chargeTrFragment) outputs

createInfoP = do
       outs <- readShell "ls " ++ folder ++ "/*.out"
       let outputs = lines outs
       pids <- mapM (\x -> async $ genInfoFile chargeTrFragment x) outputs
       mapM wait pids


rdInfoFile  :: String -> IO(Dinamica)
rdInfoFile fn = do
    cont <- readFile fn
    let (aN:aT:a:b:c:d:e:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
        atomN  = read (head aN) :: Int
        enepop = a:b:c:d:e:[]
        floats = map (map (\x -> read x :: Double)) enepop
        tupla  (a:b:c:d:e:[]) = (a,b,c,d,e)
        tuplaEnerPop = tupla floats
        coord1 = parseTriplet $ unlines f
        oscStr = map (\x-> read x :: Double) g
        charT  = map (\x-> read x :: Double) h
    return $ Dinamica fn atomN aT tuplaEnerPop coord1 oscStr charT

genInfoFile :: [Int] -> String -> IO ()
genInfoFile chargeTrFragment fn = do
    atomNS                  <- readShell $ "grep -B3 'InterNuclear Distances' " ++ fn ++ " | head -1 | awk '{print $1}'"
    let atomNumber          = read atomNS :: Int
        grepLength          = show $ atomNumber + 3
    atomTS                  <- readShell $ "grep -A" ++ grepLength ++ " ' Cartesian Coordinates' " ++ fn ++ " | tail -" ++ (show atomNumber) ++ " | awk '{print $2}'"
    energiesPop             <- mapM (\a -> readShell $ "grep Gnuplot " ++ fn ++ " | awk '{print $" ++ (show a) ++ "}' | awk 'NR % 200 == 0'") [2,3,4,5,6]
    coordinates             <- readShell $ "grep -A" ++ grepLength ++ " '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
    oscStr                  <- readShell $ "grep -A2 'Osc. strength.' " ++ fn ++ " | awk 'NR % 4 == 3' | awk '{print $3}'"
    chargeTr                <- readShell $ "awk '/Mulliken population Analysis for root number: 1/ {flag=1;next} /Expectation values of various properties for root number:  1/ {flag=0} flag {print}' " ++ fn ++ " | grep N-E | sed s/N-E//" 
    let infoname            = (takeWhile (/= '.') fn ) ++ ".info"
        div                 = "DIVISION\n"
        atomTS'             = unlines $ map (\x -> head x :[]) $ lines atomTS 
        energiesPop'        = concat $ intersperse div energiesPop
        separateString      = fmap words $ lines chargeTr
        dividedGeometries   = chunksOf atomNumber (concat separateString)
        toDouble            = map (map (\x -> read x :: Double)) dividedGeometries
        chargeTrFragmentI   = map pred chargeTrFragment
        sumUp4CT x          = sum $ map (x!!) chargeTrFragmentI
        chargeTr'           = unlines $ map show (map sumUp4CT toDouble)
        wholefile           = atomNS ++ div ++ atomTS' ++ div ++ energiesPop' ++ div ++ coordinates ++ div ++ oscStr ++ div ++ chargeTr'
    writeFile infoname wholefile

parseTriplet :: String -> [Vec Double]
parseTriplet = fmap (Vec .fmap (readDouble) . words) . lines
     where readDouble = \x -> 0.529177249 * (read x :: Double)



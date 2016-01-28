{-# Language QuasiQuotes #-}
module ParseInput where

import Data.Char (toUpper)
import Data.List.Split (chunksOf)

import Functions
import DataTypes
import Verbatim
import VerbatimParser


data Task = DihedralSingle [Int] 
          | Bla [[(Int, Int)]]
          | DihedralS [Int] 
          | EnergiesPopulation
          | Trajectories
          | Internal [Int]
          | Charge (Root,[Int])
          deriving Show

data Inputs = Inputs { 
     getfolder              :: String,
     getTasks               :: [Task],
     getgnuplotOptions      :: String
     } deriving Show

inputDefault = Inputs { getfolder = ".", getTasks = [], getgnuplotOptions = "set terminal pngcairo size 1224,830 enhanced font \", 12\""}

parseInput :: FilePath -> IO (Inputs)
parseInput fnn = do
     fn  <- correctFolderName fnn 
     a   <- readFile (fn ++ "/input")
     let tasks = readTasks a 
         input = inputDefault { getfolder = fn, getTasks = tasks} 
     return input

readTasks :: String -> [Task]
readTasks x = let
  noWhiteLines = filter (/= []) $ mosaic x
  noComments = filter (\x -> (head $ head x) /= '#') noWhiteLines
  in map readTask noComments

readTask :: [String] -> Task
readTask line = let 
  noEqual = filter (/= "=") line 
  name  = map toUpper $ head noEqual
  atoms = tail noEqual
  task  = case name of
     "DIHEDRALS"          -> let indexes = map readI atoms
                             in DihedralS indexes
     "BLA"                -> let indexes = map readI atoms
                             in Bla (takeBlaFormat indexes)
     "ENERGIESPOPULATION" -> EnergiesPopulation
     "TRAJECTORIES"       -> Trajectories
     "INTERNAL"           -> let indexes = map readI atoms
                             in Internal indexes
     "CHARGE"             -> let root  = head atoms
                                 readRoot = read root :: Root
                                 atomI = tail atoms
                                 indexes = map readI atomI
                             in Charge (readRoot,indexes)
     otherwise        -> error "This keyword does not exist, double check your input file"
  in task
  
takeBlaFormat :: [Int] -> [[(Int, Int)]]
takeBlaFormat x = let
  doubles = chunksOf 2 x
  singles = chunksOf 2 $ init (tail x)
  transformInTuple [x,y] = (x,y)
  doublesT = map transformInTuple doubles
  singlesT = map transformInTuple singles
  in [doublesT,singlesT]

inputTempl = [verbatim|
write your tasks here !!!
|]

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
      let content = printVerbatim inputTempl
      putStrLn $ "Template input file: " ++ fn ++ " written."
      writeFile fn content

fromTasksToPlottables :: [Task] -> [Plottable]
fromTasksToPlottables tasks = map fromTaskToPlottable tasks

fromTaskToPlottable :: Task -> Plottable
fromTaskToPlottable task = case task of
  EnergiesPopulation -> EnergyPop 
  otherwise          -> error "what the fuck"





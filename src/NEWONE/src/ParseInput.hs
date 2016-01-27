{-# Language QuasiQuotes #-}
module ParseInput where

import Data.Char (toUpper)
import Data.List.Split (chunksOf)

import Functions
import Verbatim
import VerbatimParser


data Tasks = Tasks [Task] deriving Show

data Task = DihedralSingle [Int] 
          | Bla [[(Int, Int)]]
          | DihedralGlobal [Int] 
          | EnergiesPopulation
          | Trajectories
          | Internal [Int]
          | Charge (String,[Int])
          deriving Show

a = parseInput "input"

fn="input"

parseInput  fn = do
     a <- readFile fn
     let tasks = readTasks a 
     return tasks

readTasks x = let
  noWhiteLines = filter (/= []) $ mosaic x
  noComments = filter (\x -> (head $ head x) /= '#') noWhiteLines
  in map readTask noComments

readTask line = let 
  noEqual = filter (/= "=") line 
  name  = map toUpper $ head noEqual
  atoms = tail noEqual
  task  = case name of
     "DIHEDRALSINGLE"     -> let indexes = map readI atoms
                             in DihedralSingle indexes
     "DIHEDRALGLOBAL"     -> let indexes = map readI atoms
                             in DihedralGlobal indexes
     "BLA"                -> let indexes = map readI atoms
                             in Bla (takeBlaFormat indexes)
     "ENERGIESPOPULATION" -> EnergiesPopulation
     "TRAJECTORIES"       -> Trajectories
     "INTERNAL"           -> let indexes = map readI atoms
                             in Internal indexes
     "CHARGE"             -> let root  = head atoms
                                 atomI = tail atoms
                                 indexes = map readI atomI
                             in Charge (root,indexes)
     otherwise        -> error "This keyword does not exist, double check your input file"
  in task
  
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

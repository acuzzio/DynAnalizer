module GnuplotZ where

import System.ShQQ
import System.Process
import Data.List
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec (parseFromFile)
import Data.Functor.Identity

import CreateInfo
import Inputs

data PlotType = Pop | Ene | Dyn deriving (Eq,Show)

energiesPopulation = do
       a <- readShell $ "ls " ++ folder ++ "/*.info"
       let files = lines a
       mapM_ graficami files

graficami file = do
   dina <- rdInfoFile file
   let popEne = getEnergies dina
       dt     = getDT dina 
       rlxRoot= getStartRlxRt dina
   writeGnuplots dt rlxRoot file popEne 
   system "gnuplot < gnuplotScript"
   system "rm sdafrffile* gnuplotScript"
   print "done"

writeGnuplots :: Double -> Int -> FilePath -> [[Double]] -> IO()
writeGnuplots dt rlxRoot file xss = do
      let valuesS   = map (unlines . (map show)) xss
          filenames = map (\x -> "sdafrffile" ++ (show x)) [1..]
          lengthV   = length valuesS
--      print values
      zipWithM writeFile filenames valuesS
      createGnuplotFile file dt lengthV rlxRoot

createGnuplotFile :: FilePath -> Double -> Int -> Int -> IO()
createGnuplotFile file dt' n rlxRt = do
      let fileZ  = takeWhile (/='.') file
          dt     = show dt'
          hexColo= ["#E5E5E5","#F0F8FF","#F0FFFF","#778899","#A9A9A9","#5F9EA0","#778899","#B0C4DE"]
          colors = ["green","red","cyan","blue","yellow","blueviolet","darkgoldenrod"]
          tag    = map (\x -> "S" ++ (show x)) [0..]
          header = "set title \"" ++ fileZ ++ " Population and Energies\"\nset xlabel \"fs\"\nset key outside\nset format y \"%6.3f\"\nset y2range[0:1.001]\nset output '" ++ fileZ ++ "EnergiesPopulation.png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\nplot "
          states = div (n-1) 2
          filenames = map (\x -> "sdafrffile" ++ (show x)) [1..]
          list   = (take states $ repeat Pop) ++ (take states $ repeat Ene) ++ [Dyn]
--          removerlXrootPopu = take rlxRt list ++ drop (succ rlxRt) list
--          removerlXtootfilename = take rlxRt filenames ++ drop (succ rlxRt) filenames
--          groupZ = group removerlXrootPopu
          groupZ = group list 
          jen x  = case head x of
                    Pop -> zip3 x hexColo tag
                    Ene -> zip3 x colors tag
                    Dyn -> zip3 x hexColo tag --hexColo tag does not matter
          lol    = concat $ map jen groupZ -- lol :: [(PlotType, String, String)]
--          almost = zipWith (\x y -> createPlotLine x y dt) lol removerlXtootfilename
          almost = zipWith (\x y -> createPlotLine x y dt) lol filenames
          secondPart = concat almost
          wholeFile  = header ++ secondPart
      writeFile "gnuplotScript" wholeFile

createPlotLine :: (PlotType, String, String) -> FilePath -> String -> String
createPlotLine (Pop,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 axes x1y2 w filledcurves x1 lt 1 lc rgb " ++ "\"" ++ c ++ "\"" ++ " t '" ++ d ++ " Population',"
createPlotLine (Ene,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lines lw 4 linecolor rgb " ++ "\"" ++ c ++ "\"" ++ " t " ++ "\"" ++ d ++ "\","
createPlotLine (Dyn,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lp ps 0.5 linecolor rgb \"black\" t \"RlxRoot\""

fromAUtoFemtoDT :: String -> String
fromAUtoFemtoDT dt = let read2 x = read x :: Double
                     in show ((read2 dt) / 41.34144728138643)

findRlxRT a = let len           = length a
                  states        = div (len-1) 2
                  (initA,tailA) = (init a, last a)
                  c             = zip [0..] initA
                  currentState  = fst . head $ filter (\x -> (head $ snd x) == (head tailA)) c
              in (currentState - states)

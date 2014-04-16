module GnuplotZ where

import System.ShQQ
import System.Process
import System.Directory
import Data.List
import Data.List.Split
import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec (parseFromFile)
import Data.Functor.Identity

import CreateInfo
import IntCoor
--import Filters
import Functions
import ParseInput
import DataTypes


plotEnergiesPopulations :: Inputs -> IO ()
plotEnergiesPopulations inputs = do
       let folder = getfolder inputs
       a <- readShell $ "ls INFO/*.info"
       let files = lines a
       mapM_ (plotEnergiesPopulation inputs) files
       putStrLn $ "\nYou can find those graphics into: " ++ folder ++ "/EnePop\n"

plotEnergiesPopulation :: Inputs -> FilePath -> IO ()
plotEnergiesPopulation inputs file = do
   dina <- rdInfoFile file
   let popEne = getEnergies dina
       dt     = getDT dina 
       rlxRoot= getStartRlxRt dina
       eneFol = "EnePop"
   writeGnuplots dt rlxRoot file popEne 
   createDirectoryIfMissing True eneFol
   system "gnuplot < gnuplotScript"
   system "rm sdafrffile* gnuplotScript"
   system $ "mv INFO/*EnergiesPopulation.png " ++ eneFol
   putStrLn $ file ++ ": done"

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
-- wanna new colors? http://hexcolorgenerator.com/          
          hexColo= ["#FFF7F7","#F7FFF7","#E5FFFF","#FFF7F7","#FFFFF7","#F9F7FF","#FFF7FF"]
          colors = ["#FF0600","#06FF00","#00FFFF","#FFB400","#FFF600","#4E00FF","#FF00FC"]
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
createPlotLine (Dyn,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lp ps 1 linecolor rgb \"black\" t \"RlxRoot\""

fromAUtoFemtoDT :: String -> String
fromAUtoFemtoDT dt = show ((read2 dt) / convFStoAU)

findRlxRT a = let len           = length a
                  states        = div (len-1) 2
                  (initA,tailA) = (init a, last a)
                  c             = zip [0..] initA
                  currentState  = fst . head $ filter (\x -> (head $ snd x) == (head tailA)) c
              in (currentState - states)

plotBondAngleDihedrals :: Inputs -> [Int] -> IO()
plotBondAngleDihedrals inputs xs = do
   let folder = getfolder inputs
   a <- readShell $ "ls INFO/*.info"
   let files = lines a        
   mapM_ (\x -> plotBondAngleDihedral inputs x xs) files
   putStrLn $ "\nYou can find those graphs into folder: " ++ folder ++ "/\n"

plotBondAngleDihedral :: Inputs -> FilePath -> [Int] -> IO()
plotBondAngleDihedral inputs fn xs = do
   case length xs of
        2         -> extractBAD inputs fn xs bond "Bond"
        3         -> extractBAD inputs fn xs angle "Angle"
        4         -> extractBAD inputs fn xs dihedral "Dihedral"
        otherwise -> putStrLn errorMsg

errorMsg = "a list of 2 -> bond\na list of 3 -> angle\na list of 4 -> dihedral"

extractBAD :: Inputs -> FilePath -> [Int] -> ([Vec Double] -> Double) -> String -> IO () 
extractBAD inputs fn atomL fun label = do
  let folder = getfolder inputs
  dina <- rdInfoFile fn
  let coord  = getCoordinates dina
      dt     = getDT dina
      atomN  = getAtomN dina
      atomLI = map pred atomL
      coordS = chunksOf atomN coord
      fileN  = takeWhile (/= '.') fn
      values = map fun $ map (\x -> map ( x !!) atomLI) coordS
      smLab  = zipWith (++) (getAtomT dina) (map show [1..])     -- [C1, C2, C3, N4, H5, C6]
      rightL = label ++ " " ++ (unwords $ map (smLab!!) atomLI)  -- "Dihedral C1 C2 C3 N4"
      fnLabe = fileN ++ (filter (/= ' ') rightL)                 -- "folder/traj054DihedralC1C2C3N4"
      limRan = if label == "Dihedral" then "set yrange [-180:180]\n" else ""
      pngFol = filter (/= ' ') rightL         -- "DihedralC1C2C3N4"
      header = "set title \"" ++ rightL ++ "\"\nset xlabel \"fs\"\nset key off\nset format y \"%6.2f\"\nset output '" ++ fnLabe ++ ".png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\n" ++ limRan ++ "plot \"" ++ (fnLabe ++ "GnupValues") ++ "\" u ($0*" ++ (fromAUtoFemtoDT (show dt)) ++ "):1 w lines"
  writeFile (fnLabe ++ "gnuplotScript") header
  writeFile (fnLabe ++ "GnupValues") $ unlines $ map show values
  createDirectoryIfMissing True pngFol
  system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
  system $ "rm " ++ (fnLabe ++ "GnupValues") ++ " " ++ (fnLabe ++ "gnuplotScript")
  system $ "mv " ++ fnLabe ++ ".png " ++ pngFol
  putStrLn $ fileN ++ ": done"

-- THIS WORKS WITH JUST 2 ROOTS, ARREGLALOOO
gnuplotG input label plotThis atd = do
  let folder    = getfolder input
      fileN     = folder ++ label 
      title     = folder ++ " " ++ label ++ " " ++ (show plotThis)
      pngName   = folder ++ label ++ (show plotThis) ++ ".png"
      lw        = "2"
      ps        = "3"
      plottable = getListToPlot input
      rightInd  = show $ (findInd plotThis plottable) + 1 -- in GNUPLOT you cannot use index starting from 0...
      header    = "set title \"" ++ title ++ "\"\nset output '" ++ pngName ++ "'\nset terminal pngcairo size 2048,1060 enhanced font \", 25\"\nset key off\n"
      plotLine  = "plot \"" ++ fileN ++ "\" u 2:" ++ rightInd ++ " lw " ++ lw ++" linecolor rgb \"black\" w lines, \"" ++ fileN ++ "01\" u 2:" ++ rightInd ++ " pt 7 ps " ++ ps ++ " w p, \"" ++ fileN ++ "10\" u 2:" ++ rightInd ++ " pt 7 ps " ++ ps ++ " w p"
      wholeScript = header ++ plotLine
  writeFile (fileN ++ "gnuplotScript") wholeScript
  system $ "gnuplot < " ++ (fileN ++ "gnuplotScript")
  


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
       let files    = lines a
           chunks   = chunksOf 10 files
       putStrLn $ "Making energy population graphics"
       sequence_ $ fmap (parallelProcFiles (plotEnergiesPopulation inputs)) chunks   -- PARALLEL STUFF : D 
--       mapM_ (plotEnergiesPopulation inputs) files
       putStrLn $ "You can find energy/population graphics into: " ++ folder ++ "/EnePop\n"

plotEnergiesPopulation :: Inputs -> FilePath -> IO ()
plotEnergiesPopulation input file = do
   dina <- rdInfoFile file
   let popEne = getEnergies dina
       dt     = getDT dina 
       rlxRoot= getStartRlxRt dina
       eToT   = getTotEn dina
       eneFol = "EnePop"
       fileZ  = takeWhile (/='.') file
   writeGnuplots input dt rlxRoot file popEne eToT
   createDirectoryIfMissing True eneFol
   system $ "gnuplot < " ++ fileZ ++ "gnuplotScript"
   system $ "rm " ++ fileZ ++ "temptemp* " ++ fileZ ++ "gnuplotScript"
   system $ "mv " ++ fileZ ++ "EnergiesPopulation.png " ++ eneFol
   putStrLn $ file ++ ": done"

writeGnuplots :: Inputs -> Double -> Int -> FilePath -> [[Double]] -> [Double] -> IO()
writeGnuplots input dt rlxRoot file xss eTot = do
      let valuesS   = map (unlines . (map show)) (xss ++ [eTot])
          fileZ  = takeWhile (/='.') file
          filenames = map (\x -> fileZ ++ "temptemp" ++ (show x)) [1..]
          lengthV   = length valuesS
--      print values
      zipWithM writeFile filenames valuesS
      createPopEneGnuplotFile input file dt lengthV rlxRoot

createPopEneGnuplotFile :: Inputs -> FilePath -> Double -> Int -> Int -> IO()
createPopEneGnuplotFile input file dt' n rlxRt = do
      let gplOpt = getgnuplotOptions input
          fileZ  = takeWhile (/='.') file
          dt     = show dt'
-- wanna new colors? http://www.2createawebsite.com/build/hex-colors.html#colorscheme
--          hexColo= ["#FFF7F7","#F7FFF7","#E5FFFF","#FFF7F7","#FFFFF7","#F9F7FF","#FFF7FF"]
          hexColo= ["#FF0600","#06FF00","#00FFFF","#FFB400","#FFF600","#4E00FF","#FF00FC"]
          colors = ["#FF0600","#06FF00","#00FFFF","#FFB400","#FFF600","#4E00FF","#FF00FC"]
          tag    = map (\x -> "S" ++ (show x)) [0..]
          header = "set title \"" ++ fileZ ++ " Population and Energies\"\nset xlabel \"fs\"\nset key outside\nset format y \"%6.3f\"\nset y2range[0:1.001]\nset output '" ++ fileZ ++ "EnergiesPopulation.png'\nset style fill transparent solid 0.2 noborder\n" ++ gplOpt ++ "\nplot "
          states = div (n-2) 2
          filenames = map (\x -> fileZ ++ "temptemp" ++ (show x)) [1..]
          list   = (take states $ repeat Pop) ++ (take states $ repeat Ene) ++ [Dyn] ++ [Tot]
--          removerlXrootPopu = take rlxRt list ++ drop (succ rlxRt) list
--          removerlXtootfilename = take rlxRt filenames ++ drop (succ rlxRt) filenames
--          groupZ = group removerlXrootPopu
          groupZ = group list 
          jen x  = case head x of
                    Pop -> zip3 x hexColo tag
                    Ene -> zip3 x colors tag
                    Dyn -> zip3 x hexColo tag --hexColo tag does not matter here
                    Tot -> zip3 x hexColo tag --hexColo tag does not matter here
          lol    = concat $ map jen groupZ -- lol :: [(PlotType, String, String)]
--          almost = zipWith (\x y -> createPlotLine x y dt) lol removerlXtootfilename
          almost = zipWith (\x y -> createPlotLine x y dt) lol filenames
          secondPart = concat almost
          wholeFile  = header ++ secondPart
      writeFile (fileZ ++ "gnuplotScript") wholeFile

createPlotLine :: (PlotType, String, String) -> FilePath -> String -> String
createPlotLine (Pop,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 axes x1y2 w filledcurves x1 lt 1 lc rgb " ++ "\"" ++ c ++ "\"" ++ " t '" ++ d ++ " Population',"
createPlotLine (Ene,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lines lw 4 linecolor rgb " ++ "\"" ++ c ++ "\"" ++ " t " ++ "\"" ++ d ++ "\","
createPlotLine (Dyn,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w lp ps 0.5 linecolor rgb \"black\" t \"RlxRoot\","
createPlotLine (Tot,c,d) b dt = "\"" ++ b ++ "\"" ++ " u ($0*" ++ (fromAUtoFemtoDT dt) ++ "):1 w l linecolor rgb \"black\" t \"Total Energy\""

fromAUtoFemtoDT :: String -> String
fromAUtoFemtoDT dt = show ((read2 dt) / convFStoAU)

findRlxRT a = let len           = length a
                  states        = div (len-1) 2
                  (initA,tailA) = (init a, last a)
                  c             = zip [0..] initA
                  currentState  = fst . head $ filter (\x -> (head $ snd x) == (head tailA)) c
              in (currentState - states)

-- This plots single trajectory graphics for Bonds Angles and Dihedrals
plotBondAngleDihedrals :: Inputs -> [Int] -> IO()
plotBondAngleDihedrals inputs xs = do
  let folder = getfolder inputs
  a <- readShell $ "ls INFO/*.info"
  let files    = lines a        
      chunks   = chunksOf 10 files
  sequence_ $ fmap (parallelProcFiles (\x -> plotBondAngleDihedral inputs x xs)) chunks   -- PARALLEL STUFF : D 
--   mapM_ (\x -> plotBondAngleDihedral inputs x xs) files
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
extractBAD input fn atomL fun label = do
  let folder = getfolder input
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
      pngFol = filter (/= ' ') rightL                            -- "DihedralC1C2C3N4"
      gplOpt = getgnuplotOptions input
      header = "set title \"" ++ rightL ++ "\"\nset xlabel \"fs\"\nset key off\nset format y \"%6.2f\"\nset output '" ++ fnLabe ++ ".png'\n" ++ gplOpt ++ "\n" ++ limRan ++ "plot \"" ++ (fnLabe ++ "GnupValues") ++ "\" u ($0*" ++ (fromAUtoFemtoDT (show dt)) ++ "):1 w lines"
  writeFile (fnLabe ++ "gnuplotScript") header
  writeFile (fnLabe ++ "GnupValues") $ unlines $ map show values
  createDirectoryIfMissing True pngFol
  system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
  system $ "rm " ++ (fnLabe ++ "GnupValues") ++ " " ++ (fnLabe ++ "gnuplotScript")
  system $ "mv " ++ fnLabe ++ ".png " ++ pngFol
  putStrLn $ fileN ++ ": done"

gnuplotG :: Inputs -> String -> String -> Int -> [Plottable] -> Plottable -> AllTrajData -> IO ()
gnuplotG input label label2 nRoot datalabels plotThis atd = do
  let folder        = getfolder input
      nRootI        = nRoot-1
      fileN         = folder ++ label ++ label2
      title         = folder ++ " " ++ label ++ " " ++ label2
      pngName       = folder ++ label ++ label2 ++ ".png"
      lw            = "1"
      ps            = "2"
      gplOpt        = getgnuplotOptions input
      allJumps      = [(show x) ++ (show y) | x <- [0.. nRootI], y <- [0.. nRootI], x/=y]
      rightIndex    = rightInd datalabels plotThis nRoot
      getHOP root   = filter (\x -> x /= []) $ map (filter (\x-> x!!3 == root)) atd -- this x!!3 supposes that Jump is on fourth column of data files
      getHOPs       = map getHOP allJumps
--  putStrLn $ "WOOOOOOOOW " ++ rightIndex ++ " " ++ show nRootI
  writeFile fileN $ unlines $ map unlines $ map (map unwords) atd
  mapM_ (\x -> writeFile (fileN ++ fst x) $ writeF (getHOPs !! snd x)) $ zip allJumps [0..]
  let header        = "set title \"" ++ title ++ "\"\nset output '" ++ pngName ++ "'\n" ++ gplOpt ++ "\nset key off\n" ++ (rangeOption plotThis) ++ "\n"
      hopPlotLine l = ", \"" ++ fileN ++ l ++ "\" u 2:" ++ rightIndex ++ " pt 7 ps " ++ ps ++ " w p"
      allHopsPlotL  = concat $ map hopPlotLine allJumps
      wholePlotLine = "plot \"" ++ fileN ++ "\" u 2:" ++ rightIndex ++ " lw " ++ lw ++" linecolor rgb \"black\" w lines" ++ allHopsPlotL
      wholeScript = header ++ wholePlotLine
      gnuplScriptName = fileN ++ "gnuplotScript"
  writeFile gnuplScriptName wholeScript
  let folderName = "GlobalGraphics"
  createDirectoryIfMissing True folderName
  system $ "gnuplot < " ++ (fileN ++ "gnuplotScript 2> /dev/null")
  system $ "mv " ++ gnuplScriptName ++ " " ++ folderName
  system $ "mv *" ++ label2 ++ "* " ++ folderName
  putStrLn "done"

rangeOption plotThis = case plotThis of
        BlaPlot _ -> "set yrange [-0.5:0.5]"
        otherwise -> ""

-- rightInd finds the right column of values in data files
rightInd :: [Plottable] -> Plottable -> Int -> String
rightInd datalabels plotThis nroot = let -- say we have [En,Int[1,2],Int[1,2,3,4]] (Int[1,2])  
  index = findIndM2 plotThis datalabels -- this is 1
  shortPlottableList = take index datalabels -- this is [En,Int[1,2]]
  column = map (howManyColumns nroot) shortPlottableList -- with nroot=2 this is [5,1]
  in show $ (sum column) + 2 + 1 -- this is "10" -> the right column number of Int[1,2] in data files

howManyColumns nroot x = case x of
  EnergyPop      -> (nroot * 2) + 1 + 2 -- even Jump and Root
  InternalPlot _ -> 1
  BlaPlot      _ -> 1

----gnuplotCT :: Inputs -> String -> Plottable -> AllTrajData -> Double -> IO()
--gnuplotCT input label plotThis atd thresh = do
--  let folder        = getfolder input
--      fileN         = "chargeTr" ++ folder ++ (show thresh) ++ label
--      fileScript    = "chargeTr" ++ folder ++ (show thresh) ++ label ++ (show plotThis)
--      title         = folder ++ " " ++ label ++ " " ++ (show thresh) ++ " " ++ (show plotThis)
--      pngName       = folder ++ label ++ (show plotThis) ++ (show thresh) ++ ".png"
--      lw            = "1"
--      ps            = "1"
--      isomK         = getisomType input
--      rangeOption   = case plotThis of
--                         Bla   -> "set yrange [-0.5:0.5]"
--                         Delta -> "set yrange [-150:150]"
--                         otherwise -> case isomK of
--                                        Cis   -> "set yrange [-300:300]"
--                                        Trans -> "set yrange [-540:180]"
--      plottable     = getListToPlot input
--      rightInd      = show $ (findInd plotThis plottable) + 1 
--      gplOpt        = getgnuplotOptions input
--      nRootI        = pred $ getnRoot input
--      allJumps      = [(show x) ++ (show y) | x <- [0.. nRootI], y <- [0.. nRootI], x/=y]
--      header        = "set title \"" ++ title ++ "\"\nset output '" ++ pngName ++ "'\n" ++ gplOpt ++ "\nset key off\n" ++ rangeOption ++ "\n"
--      hopPointColor = concat $ repeat ["green","blue","red","yellow","grey"]
--      hopPlotLine hopType color = ", \"" ++ fileN ++ hopType ++ "\" u 2:" ++ rightInd ++ " linecolor rgb \"" ++ color ++ "\" pt 7 ps " ++ ps ++ " w p"
--      allHopsPlotL  = concat $ zipWith (\x y -> hopPlotLine x y) allJumps hopPointColor
--      wholePlotLine = "plot \"" ++ fileN ++ "HI\" u 2:" ++ rightInd ++ " lw " ++ lw ++" linecolor rgb \"red\" w lines, \"" ++ fileN ++ "LO\" u 2:" ++ rightInd ++ " lw " ++ lw ++" linecolor rgb \"black\" w lines" ++ allHopsPlotL
--      wholeScript   = header ++ wholePlotLine 
--  writeFile (fileScript ++ "gnuplotScript") wholeScript
--  system $ "gnuplot < " ++ (fileScript ++ "gnuplotScript 2> /dev/null")
--  let folderCT = "ChargeTranfGraphs"
--  createDirectoryIfMissing True folderCT
--  system $ "mv " ++ pngName ++ " " ++ folderCT
--
--
--

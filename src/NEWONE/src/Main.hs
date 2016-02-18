module Main where

--import Control.Concurrent.Async
import Control.Monad
--import Control.Exception
--import Data.List
import System.Console.GetOpt
--import System.Console.ANSI
import System.Directory
import System.Environment (getArgs)
import System.Exit
--import System.IO
--import System.IO.Error
import System.ShQQ

import CalculateData
import CreateInfo
import DataTypes
import Filters
import GnuplotZ
import ParseInput
--import Statistics
import Trajectories
--import Quickies
import Functions
--import MolcasParser
--import InfoParser
--
-- Main function, that takes care of argouments. If you launch without arguments, it will display help, then checks which flags you call and pass them to getOpt. If no flags is recognized ( [] ) then display the help as well. the successfull computation here is the mapM_ of getExpressions
main :: IO()
main = do
  gotArgs <- getArgs
  if gotArgs == [] 
     then do
       useMessage
     else do
       (flags,args,_) <- return $ getOpt RequireOrder options gotArgs
       when (Help `elem` flags) $ useMessage >> exitSuccess
       if flags == []
          then putStrLn "\nI cannot understand ... Why r u overestimating me ? To get some help:\n\n$ DynAnalyzer -h\n\n "
          else mapM_ getExpression flags
  
-- options is of type [OptDescr Flag] and it has his own istance of show, so we do not have to care about his print formatting. here I just want to display a small message and show the options  
useMessage = putStrLn $ usageInfo startMessage options

-- welcoming message
welcome = "\n\nWelcome to DynAnalyzer, a tool to get informations from Molcas Molecular Dynamics with Tully\n\n\nVERSION 0.2.0\n"

startMessage = welcome ++ "\nThese are the options available:"

-- avaiable command line direct option in this program. Each option has 
-- 1) "LETTER"
-- 2) ["quick help string"]
-- 3) (some options)
-- 4) "a long explanation line that appears in printout"
--
options :: [OptDescr Flag]
options = [
   Option "h" ["help"]
     (NoArg Help)
     "display this message",
   Option "b" ["createInfoQMBinary"]
     (ReqArg CreateInfoBin "OUTPUTPATH")
     "it creates info files from Molcas QM outputs if they're binary. To be used with quotation marks: 'folder/*.log' or '*/*.out'",
   Option "o" ["createInfoQM"]
     (ReqArg CreateInfo "OUTPUTPATH")
     "it creates info files from Molcas QM outputs. To be used with quotation marks: 'folder/*.log' or '*/*.out'",
   Option "q" ["createInfoQMMM"]
     (ReqArg CreateInfoQMMM "OUTPUTPATH")
     "it creates info files from Molcas QM/MM outputs. To be used with quotation marks: 'folder/*.log' or '*/*.out'",
   Option "t" ["CheckInfo"]
     (ReqArg CheckInfo "INFOPATH")
     "it test for consistency in info files inside specified folder. To be used with quotation marks: 'folder/*.info' or '*/*.info'",
   Option "f" ["folder"]
     (ReqArg InputFile "ProjectFolder")
     "It will run the program using the information into FOLDER. In case it does not exist, a template one will be created"
 ]

-- So this is the main function for command line use of dynanalyzer. Just takes the flag, case on the datatype Flag and do what the option is supposed to do. All of them MUST be IO(). The purpose of the program is to take a bunch of big files (molcas outputs) and exctract just the information we need for the analysis into info files (waaay smaller). Then do some analysis on data.

getExpression :: Flag -> IO ()
getExpression flag = 
  case flag of
    CreateInfo path -> do     -- this takes care of the creation of INFO files part.
       createInfoQM Normal path
    CreateInfoBin path -> do  -- this takes care of the creation of INFO files when binary
       createInfoQM Binary path
    CreateInfoQMMM path -> do -- same, but with QM/MM files.
       createInfoQMMM path
    InputFile fn    -> do   -- this executes the task of the folder fn. If fn deos not exists, it creates a new template folder. 
       aa <- doesDirectoryExist fn
       case aa of
          True -> do  -- execute the input 
              putStrLn welcome
              input <- parseInput fn
              checkFolder fn
              setCurrentDirectory fn
              executeTasks input
              putStrLn "Tasks done."
          False -> do
              createDirectory fn
              putStrLn $ "\nFolder " ++ fn ++ " does not exist. So I created it.\n"
              createDirectory $ fn ++ "/INFO"
              writeInputTemplate $ fn ++ "/input"
              putStrLn $ "\nNow you should copy your info files into folder " ++ fn ++ "/INFO/ and modify " ++ fn ++ "/input according to your system\n"
    CheckInfo path   -> do     -- this triggers the tests on info files
       checkInfoFiles path

checkFolder :: FilePath -> IO ()
checkFolder folder = do
  infos <- readShell $ "ls " ++ folder ++ "/INFO/*.info"
  let infosNames = lines infos
      infosNum   = length infosNames
      infofolder = folder ++ "INFO"
  case infosNum of
    0 -> putStrLn $ "I did not find any Info files into " ++ infofolder ++ " folder !\n"
    1 -> putStrLn $ "I found a single info file into " ++ infofolder ++ " folder.\n"
    otherwise -> putStrLn $ "I found " ++ show infosNum ++ " info files into " ++ infofolder ++ " folder.\n"

getRootNumber :: FilePath -> IO(Int)
getRootNumber folder = do
  --infos <- readShell $ "ls " ++ folder ++ "/INFO/*.info"
  infos <- readShell $ "ls INFO/*.info"
  let infosNames = lines infos
      first      = head infosNames
  a <- rdInfoFile first    
  return $ getRootN a

executeTasks :: Inputs -> IO () 
executeTasks input = do
  nroot <- getRootNumber $ getfolder input 
  let tasks = getTasks input
  plottableForDataFile <- mapM (executeSingleTaskPreData input) tasks
  let plottableForDataFileNoEmpty = filter (/= Empty) plottableForDataFile
  stringOnBox "Creating DATA files:"
  createDATAs input plottableForDataFileNoEmpty
  atd <- readerData
  mapM_ (executeSingleTaskPostData input plottableForDataFileNoEmpty nroot atd) tasks

executeSingleTaskPreData :: Inputs -> Task -> IO (Plottable)
executeSingleTaskPreData input task = do
  case task of
    EnergiesPopulation -> do
                          stringOnBox "Energy/populations graphics requested"
                          plotEnergiesPopulations input
                          return EnergyPop
    Trajectories       -> do
                          stringOnBox "Trajectories requested"
                          genTrajectories input
                          return Empty
    Internal     xs    -> do
                          stringOnBox $ "Internal coordinate analysis requested on " ++ show xs
                          plotBondAngleDihedrals input xs
                          return $ InternalPlot xs
    Bla blai           -> do 
                          stringOnBox $ "BLA graphics requested on " ++ show blai
                          return $ BlaPlot blai
    Dihedrals (a,b)    -> do
                          stringOnBox $ "Dihedral analysis requested on alpha:" ++ show a ++ " and beta: " ++ show b
                          return $ DihAna (a,b)
--    DihedralSingle xs
--    DihedralGlobal xs 
--    Internal xs
--    Charge (label,xs  

executeSingleTaskPostData :: Inputs -> [Plottable] -> Int -> AllTrajData -> Task -> IO ()
executeSingleTaskPostData input plottable nroot atd task = do
  case task of
    EnergiesPopulation  -> return ()
    Trajectories        -> return ()
    Internal     xs     -> do
       stringOnBox $ "Making global graphics for internal coordinate " ++ show xs
       let firstLabel   = "All"
           secondLabel  = show $ InternalPlot xs      
       gnuplotG input firstLabel secondLabel nroot plottable (InternalPlot xs) atd     
    Bla         blai    -> do
       stringOnBox $ "Making global graphics for BLA using " ++ show blai
       let firstLabel   = "All"
           secondLabel  = show $ BlaPlot blai
       gnuplotG input firstLabel secondLabel nroot plottable (BlaPlot blai) atd
    Dihedrals (alpha,beta) -> do
       stringOnBox $ "Dihedral Analysis -> alpha:" ++ show alpha ++ " and beta: " ++ show beta 
       dihedralAnalysis alpha beta input atd



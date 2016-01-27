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

--import CalculateData
import CreateInfo
import DataTypes
--import Filters
--import GnuplotZ
import ParseInput
--import Statistics
--import Trajectories
--import Quickies
--import Functions
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
startMessage = "\n\nWelcome to DynAnalyzer, a tool to get informations from Molcas Molecular Dynamics with Tully\n\n\nTHIS IS THE SURFACE HOP VERSION!!!!\n\n\nThose are the options avaiable:"

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
    InputFile fn     -> do   -- this creates a new folder to work in, or set the working folder to call a menu.
       aa <- doesDirectoryExist fn
       case aa of
          True -> do  -- execute the input 
                  tasks <- parseInput (fn ++ "/input")
                  putStrLn $ show tasks
          False -> do
              createDirectory fn
              putStrLn $ "\nFolder " ++ fn ++ " does not exist. So I created it.\n"
              createDirectory $ fn ++ "/INFO"
              writeInputTemplate $ fn ++ "/input"
              putStrLn $ "\nNow you should copy your info files into folder " ++ fn ++ "/INFO/ and modify " ++ fn ++ "/input according to your system\n"
    CheckInfo path   -> do     -- this triggers the tests on info files
       checkInfoFiles path

checkFolder folder = do
  infos <- readShell $ "ls " ++ folder ++ "/INFO/*.info"
  let infosNames = lines infos
      infosNum   = length infosNames
  case infosNum of
    0 -> putStrLn $ "I did not find any Info files into " ++ folder ++ " folder !"
    otherwise -> putStrLn $ "I found " ++ show infosNum ++ " info files into " ++ folder ++ " folder."

--menuGraphsEnePop input = do
--  plotEnergiesPopulations input
--  blockScreenTillPress
--
--menuGraphsBAD input = do
--  putStrLn "\nWhich graphs do you want?\n 1 ) Central Dihedral\n 2 ) Beta\n 3 ) Other\n"
--  choice1 <- getLine
--  let a = read choice1
--  case a of
--    1 -> plotBondAngleDihedrals input $ getccccList input
--    2 -> plotBondAngleDihedrals input $ getbetaList input
--    3 -> do 
--         putStrLn "\nPlease give me a list with atom numbers, like [1,2,3]:\n"
--         choice2 <- getLine
--         let a = read choice2 :: [Int]
--         plotBondAngleDihedrals input a
--    otherwise -> do 
--                 putStrLn "\nI do not like you.\n"
--                 menuGraphsBAD input
--  blockScreenTillPress
--
--menuTrajectories input = do
--  genTrajectories input
--  blockScreenTillPress
--
--menuLifeTimes2 input = do
--  let nRoot = getnRoot input
--      menu  = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) Graphic of lifetime in S" ++ (show $ pred x)) [1..nRoot]
--  putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
--  choice1 <- getLine
--  let a = read choice1
--  if a `elem` [1..nRoot]
--    then do
--      graphicLifeTime3 input a
--      blockScreenTillPress
--      --graphicLifeTime input a
--    else do
--      putStrLn "\nI do not like you.\n"
--      blockScreenTillPress
--      menuLifeTimes2 input
--
--
--menuLifeTimes input = do
--  let nRoot = getnRoot input
--  putStrLn "\nDo you know the range already or you want to print the graphic?\n\n 1 ) Graphic, please\n 2 ) Yes, I know the range\n"
--  choice1 <- getLine
--  let a = read choice1
--  case a of
--    1 -> do
--         let menu = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) Graphic of lifetime in S" ++ (show $ pred x)) [1..nRoot]
--         putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
--         choice2 <- getLine
--         let b = read choice2 :: Int
--         if b `elem` [1..nRoot] 
--            then do
--              graphicLifeTime3 input b
--              blockScreenTillPress
--            else do
--              putStrLn "\nI do not like you.\n"
--              menuLifeTimes input
--    2 -> do
--         let menu = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) S" ++ (show $ pred x)) [1..nRoot]
--         putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
--         choice3 <- getLine
--         let c = read choice3 :: Int
--         if c `elem` [1..nRoot]
--            then do
--              putStrLn $ "\nAt which step does that exponential curve start?\n"
--              choice4 <- getLine
--              let d = read choice4 :: Int
--              putStrLn $ "\nAt which step does it finish?\n"
--              choice5 <- getLine
--              let e = read choice5 :: Int
--              calculateLifeTime input c d e
--              blockScreenTillPress
--            else do
--              putStrLn "\nI do not like you.\n"
--              menuLifeTimes input
--    otherwise -> do
--                 putStrLn "\nI do not like you.\n"
--                 menuLifeTimes input
--
--menuData input = do
--  createDATAs input  
--  blockScreenTillPress
--
--menuAnalysis input = do
--  mainfilter input
--  blockScreenTillPress
--
--menuCT input = do
--  chargeTmap input 
--  blockScreenTillPress
--
--menuAll input = do 
--  plotEnergiesPopulations input 
--  plotBondAngleDihedrals input $ getccccList input 
--  genTrajectories input 
--  createDATAs input
--  graphicLifeTime3 input 2 
--  mainfilter input
--  putStrLn "Now doing the CT part:"
--  chargeTmap input
--  putStrLn "Done"
--  blockScreenTillPress
--
--byeString="\nDynAnalyzer - by AcuZZio\n"
--
--quitWithStyle input = do
--  setSGR [Reset]
--  clearScreen
--  putStrLn $ byeString ++ "\nBye Bye !!\n"
--  exitSuccess
--
--quitNoStyle = do
--  setSGR [Reset]
--  clearScreen
--  putStrLn $ byeString ++ "\nRemember, it was your fault, not mine.\n"
--  exitSuccess
--
--blockScreenTillPress = do
--  putStrLn "\nPress ENTER to go back to main menu..."
--  choice2 <- getLine
--  return ()
--
--

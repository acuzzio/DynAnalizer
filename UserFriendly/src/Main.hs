module Main where

import Control.Monad
import Control.Exception
import Data.List
import System.Console.GetOpt
import System.Console.ANSI
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Error
import System.ShQQ

import CalculateData
import CreateInfo
import DataTypes
import Filters
import GnuplotZ
import ParseInput
import Statistics
import Trajectories
import Quickies


main :: IO()
main = do
  gotArgs <- getArgs
  if gotArgs == [] 
     then do
       useMessage
     else do
       (flags,args,_) <- return $ getOpt RequireOrder options gotArgs
       when (Help `elem` flags) $ useMessage >> exitSuccess
       mapM_ getExpression flags
  
useMessage = putStrLn $ usageInfo startMessage options

startMessage = "\n\nWelcome to DynAnalyzer, a tool to get informations from Molcas Molecular Dynamics with Tully\n\nThose are the options avaiable:"

options :: [OptDescr Flag]
options = [
   Option "h" ["help"]
     (NoArg Help)
     "display this message",
   Option "c" ["createInfo"]
     (ReqArg CreateInfo "FOLDERNAME")
     "it creates info files from Molcas outputs in the specified folder",
   Option "C" ["CheckInfo"]
     (ReqArg CheckInfo "FOLDERNAME")
     "it checks for consistency in info files inside specified folder",
   Option "A" ["folder"]
     (ReqArg DoAll "ProjectFolder")
     "DO EVERYTHING in this folder",
   Option "a" ["folder"]
     (ReqArg Doall "ProjectFolder")
     "I have data files, I just need graphs in this folder",
   Option "f" ["folder"]
     (ReqArg InputFile "ProjectFolder")
     "It will run the program using the information into FOLDER. In case it does not exist, a template one will be created",
   Option "L" ["Luisma ASKED this"]
     (ReqArg Quick "ProjectFolder")
     "this option run the code into Quickies file, if you do not know what it is... you probably don't need it"
   ]

getExpression :: Flag -> IO ()
getExpression flag = 
  case flag of
    CreateInfo folder -> do
       let folderWithoutSlash = if (last folder == '/') then init folder else folder
       createInfo folderWithoutSlash
    InputFile fnn     -> do
       let fn = if (last fnn == '/') then init fnn else fnn
       aa <- doesDirectoryExist fn
       case aa of
          True -> do handle ((\_ -> quitNoStyle) :: AsyncException -> IO ()) $ goIntoMenu fn
        --  True -> do goIntoMenu fn
          False -> do
              createDirectory fn
              putStrLn $ "\nFolder " ++ fn ++ " does not exist. So I created it.\n"
              createDirectory $ fn ++ "/INFO"
              writeInputTemplate $ fn ++ "/input"
              putStrLn $ "\nNow you should copy your info files into folder " ++ fn ++ "/INFO/ and modify " ++ fn ++ "/input according to your system\n"
    CheckInfo folder   -> do
       let folderWithoutSlash = if (last folder == '/') then init folder else folder
       checkInfoFiles folderWithoutSlash
    DoAll fnn -> do
       let fn = if (last fnn == '/') then init fnn else fnn
       setCurrentDirectory fn
       inputFile <- getInputInfos "input"
       let input = inputFile { getfolder = fn }
       case (getchargeTrFragment input) == [9999] of
          True  -> do putStrLn "There is a problem into input file."
          False -> do plotEnergiesPopulations input 
                      plotBondAngleDihedrals input $ getccccList input 
                      createDATAs input
                      genTrajectories input 
                      graphicLifeTime3 input 2 
                      mainfilter input
                      putStrLn "Now doing the CT part:"
                      chargeTmap input
                      putStrLn "Done"
    Doall fnn -> do
       let fn = if (last fnn == '/') then init fnn else fnn 
       setCurrentDirectory fn
       inputFile <- getInputInfos "input"
       let input = inputFile { getfolder = fn }
       case (getchargeTrFragment input) == [9999] of
          True  -> do putStrLn "There is a problem into input file."
          False -> do mainfilter input
                      putStrLn "Now doing the CT part:"
                      chargeTmap input
                      putStrLn "Done"
    Quick fnn -> do
       let fn = if (last fnn == '/') then init fnn else fnn
       setCurrentDirectory fn
       inputFile <- getInputInfos "input"
       let input = inputFile { getfolder = fn }
       case (getchargeTrFragment input) == [9999] of
          True  -> do putStrLn "There is a problem into input file."
          False -> do luismaAsk input
                      putStrLn "Done"



--MENUUUU
goIntoMenu fn = do
  let concatNums (i, (s, _)) = " " ++ show i ++ " ) " ++ s
  setTitle "DynAnalyzer by AcuZZio"
  setSGR [SetColor Background Vivid Black, SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
  clearScreen
  curDir <- getCurrentDirectory
  setCurrentDirectory fn
  inputFile <- getInputInfos "input"
  let input = inputFile { getfolder = fn }
--  checkFolder fn
  putStrLn $ "\nSo here we are again for the folder " ++ fn ++ ". What do you want to do now ?\n"
  putStrLn . unlines $ map concatNums choices
  choice <- getLine
  case validate choice of
       Just n  -> execute (read $ choice, input)
       Nothing -> putStrLn "This option does not exist !!\n"
  setCurrentDirectory curDir
  goIntoMenu fn

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, (Inputs -> IO ())))]
choices = zip [1.. ] [
   ("I want to see Energies and Populations graphics", menuGraphsEnePop),
   ("I want to plot a Bond, an Angle or a Dihedral angle", menuGraphsBAD),
   ("I want to see MD Trajectories !", menuTrajectories),
   ("I want to create DATA files !!", menuData),
   ("I have DATA files, I want to know average lifetimes !!", menuLifeTimes2),
   ("I have DATA files, wanna do some Analysis !!", menuAnalysis),
   ("I have DATA files, wanna do some Charge Transfer graphs !!", menuCT),
   ("Wanna Do Them All !!", menuAll),
   ("Quit", quitWithStyle)
    ]

execute (n, input) = let     
     doExec ((_, (_,f)):_) = f input
     in doExec $ filter (\(i, _) -> i == n) choices

checkFolder folder = do
  infos <- readShell $ "ls " ++ folder ++ "/INFO/*.info"
  let infosNames = lines infos
      infosNum   = length infosNames
  case infosNum of
    0 -> putStrLn $ "I did not find any Info files into " ++ folder ++ " folder !"
    otherwise -> putStrLn $ "I found " ++ show infosNum ++ " info files into " ++ folder ++ " folder."

menuGraphsEnePop input = do
  plotEnergiesPopulations input
  blockScreenTillPress

menuGraphsBAD input = do
  putStrLn "\nWhich graphs do you want?\n 1 ) Central Dihedral\n 2 ) Beta\n 3 ) Other\n"
  choice1 <- getLine
  let a = read choice1
  case a of
    1 -> plotBondAngleDihedrals input $ getccccList input
    2 -> plotBondAngleDihedrals input $ getbetaList input
    3 -> do 
         putStrLn "\nPlease give me a list with atom numbers, like [1,2,3]:\n"
         choice2 <- getLine
         let a = read choice2 :: [Int]
         plotBondAngleDihedrals input a
    otherwise -> do 
                 putStrLn "\nI do not like you.\n"
                 menuGraphsBAD input
  blockScreenTillPress

menuTrajectories input = do
  genTrajectories input
  blockScreenTillPress

menuLifeTimes2 input = do
  let nRoot = getnRoot input
      menu  = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) Graphic of lifetime in S" ++ (show $ pred x)) [1..nRoot]
  putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
  choice1 <- getLine
  let a = read choice1
  if a `elem` [1..nRoot]
    then do
      graphicLifeTime3 input a
      blockScreenTillPress
      --graphicLifeTime input a
    else do
      putStrLn "\nI do not like you.\n"
      blockScreenTillPress
      menuLifeTimes2 input


menuLifeTimes input = do
  let nRoot = getnRoot input
  putStrLn "\nDo you know the range already or you want to print the graphic?\n\n 1 ) Graphic, please\n 2 ) Yes, I know the range\n"
  choice1 <- getLine
  let a = read choice1
  case a of
    1 -> do
         let menu = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) Graphic of lifetime in S" ++ (show $ pred x)) [1..nRoot]
         putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
         choice2 <- getLine
         let b = read choice2 :: Int
         if b `elem` [1..nRoot] 
            then do
              graphicLifeTime3 input b
              blockScreenTillPress
            else do
              putStrLn "\nI do not like you.\n"
              menuLifeTimes input
    2 -> do
         let menu = intercalate "\n" $ map (\x -> " " ++ (show x) ++ " ) S" ++ (show $ pred x)) [1..nRoot]
         putStrLn $ "\nWhich Root?\n\n" ++ menu ++ "\n"
         choice3 <- getLine
         let c = read choice3 :: Int
         if c `elem` [1..nRoot]
            then do
              putStrLn $ "\nAt which step does that exponential curve start?\n"
              choice4 <- getLine
              let d = read choice4 :: Int
              putStrLn $ "\nAt which step does it finish?\n"
              choice5 <- getLine
              let e = read choice5 :: Int
              calculateLifeTime input c d e
              blockScreenTillPress
            else do
              putStrLn "\nI do not like you.\n"
              menuLifeTimes input
    otherwise -> do
                 putStrLn "\nI do not like you.\n"
                 menuLifeTimes input

menuData input = do
  createDATAs input  
  blockScreenTillPress

menuAnalysis input = do
  mainfilter input
  blockScreenTillPress

menuCT input = do
  chargeTmap input 
  blockScreenTillPress

menuAll input = do 
  plotEnergiesPopulations input 
  plotBondAngleDihedrals input $ getccccList input 
  genTrajectories input 
  createDATAs input
  graphicLifeTime3 input 2 
  mainfilter input
  putStrLn "Now doing the CT part:"
  chargeTmap input
  putStrLn "Done"
  blockScreenTillPress

byeString="\nDynAnalyzer - by AcuZZio\n"

quitWithStyle input = do
  setSGR [Reset]
  clearScreen
  putStrLn $ byeString ++ "\nBye Bye !!\n"
  exitSuccess

quitNoStyle = do
  setSGR [Reset]
  clearScreen
  putStrLn $ byeString ++ "\nRemember, it was your fault, not mine.\n"
  exitSuccess

blockScreenTillPress = do
  putStrLn "\nPress ENTER to go back to main menu..."
  choice2 <- getLine
  return ()

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
import GnuplotZ
import ParseInput
import Statistics
import Trajectories


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
   Option "f" ["folder"]
     (ReqArg InputFile "ProjectFolder")
     "It will run the program using the information into FOLDER. In case it does not exist, a template one will be created"
   ]

getExpression :: Flag -> IO ()
getExpression flag = 
  case flag of
    CreateInfo folder -> createInfo folder
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
    CheckInfo folder   -> checkInfoFiles folder

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
  let content = "chargeTrFragment = [1,2,3]                       -- Here list of Atom in charge transfer fraction\nccccList   = [5,4,6,7]                           -- Here the central dihedral\nbetaList   = [3,4,6,10]                          -- Here beta angle\nblaList    = [[(1,5),(4,6),(7,8)],[(4,5),(6,7)]] -- BLA list of single bonds, list of double bonds\nisomType   = Cis                                 -- Here Cis or Trans\nnRoot      = 2                                   -- This is the number of root in the system\ndataPlot   = [Cccc, CcccCorrected, Beta, BetaCorrected, Tau, Delta, Bla, Ct, Root, Jump]\n\n"
  putStrLn $ "Template input file: " ++ fn ++ " written."
  writeFile fn content

--MENUUUU
goIntoMenu fn = do
  let concatNums (i, (s, _)) = " " ++ show i ++ " ) " ++ s
  setTitle "DynAnalyzer by AcuZZio"
  setSGR [SetColor Background Dull White, SetColor Foreground Dull Black, SetConsoleIntensity BoldIntensity]
  clearScreen
  curDir <- getCurrentDirectory
  setCurrentDirectory fn
  inputFile <- getInputInfos "input"
  let input = inputFile { getfolder = fn }
--  checkFolder fn
  putStrLn "\nSo here we are again. What do you want to do now ?\n"
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
   ("I want to see the graphics of Energies and Population", menuGraphsEnePop),
   ("I want a graphic of a Bond, an Angle or a Dihedral angle", menuGraphsBAD),
   ("I want to see Trajectories !", menuTrajectories),
   ("I want to know lifetimes !!", menuLifeTimes),
   ("I want to create DATA files !!", menuData),
   ("Quit", quitWithStyle)
    ]

execute (n, input) = let     
     doExec ((_, (_,f)):_) = f input
     in doExec $ filter (\(i, _) -> i == n) choices

checkFolder folder = do
  infos <- readShell $ "ls " ++ folder ++ "INFO/*.info"
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
              graphicLifeTime input b
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

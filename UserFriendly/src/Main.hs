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

import CreateInfo
import DataTypes
import GnuplotZ
import ParseInput
import Statistics
import Trajectories


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
   Option "i" ["input"]
     (ReqArg InputFile "INPUTFILE")
     "It will run the program using this input file. In case it does not exist, a template file will be created"
   ]

useMessage = putStrLn $ usageInfo startMessage options

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
  

getExpression :: Flag -> IO ()
getExpression flag = 
  case flag of
    CreateInfo folder -> createInfo folder
    InputFile fn      -> do
       aa <- doesDirectoryExist fn
       case aa of
          True -> do putStrLn "\nDo not use the same name as an existing folder!! \n" >> exitSuccess
          False -> do
            a <- doesFileExist fn
            case a of
            --  True     -> goIntoMenu fn
              True     -> handle ((\_ -> quitNoStyle) :: SomeException -> IO ()) $ goIntoMenu fn -- if file already exists, it enters the menu
              False    -> writeInputTemplate fn
    CheckInfo folder   -> checkInfoFiles folder

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
  let content = "folder     = o-traj1trans              -- Here Info foldername\nchargeTrFragment = [1,2,3]                       -- Here list of Atom in charge transfer fraction\nccccList   = [5,4,6,7]                           -- Here the central dihedral\nbetaList   = [3,4,6,10]                          -- Here beta angle\nblaList    = [[(1,5),(4,6),(7,8)],[(4,5),(6,7)]] -- BLA list of single bonds, list of double bonds\nisomType   = Cis                                 -- Here Cis or Trans\nnRoot      = 2                                   -- This is the number of root in the system\n\n"
  putStrLn $ "\nTemplate input file: " ++ fn ++ " written.\n"
  putStrLn "Change it as you wish, then re-run this command !! \n"
  writeFile fn content

--MENUUUU

goIntoMenu fn = do
  let concatNums (i, (s, _)) = " " ++ show i ++ " ) " ++ s
  setTitle "DynAnalyzer by Alessio Valentini"
  setSGR [SetColor Background Dull Cyan, SetConsoleIntensity BoldIntensity]
  clearScreen
  putStrLn "\nSo here we are again. What do you want to do now ?\n"
  putStrLn . unlines $ map concatNums choices
  choice <- getLine
  case validate choice of
       Just n  -> execute (read $ choice, fn)
       Nothing -> putStrLn "This option does not exist !!\n"
  goIntoMenu fn

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, (FilePath -> IO ())))]
choices = zip [1.. ] [
   ("I want to see the graphics of Energies and Population", menuGraphsEnePop),
   ("I want a graphic of a Bond, an Angle or a Dihedral angle", menuGraphsBAD),
   ("I want to see Trajectories !", menuTrajectories),
   ("I want to know lifetimes !!", menuLifeTimes),
   ("Quit", quitWithStyle)
    ]

--execute :: (Int,FilePath) -> IO ()
execute (n, fn) = let     
     doExec ((_, (_,f)):_) = f fn
     in doExec $ filter (\(i, _) -> i == n) choices

menuGraphsEnePop fn = do
  input <- getInputInfos fn
  plotEnergiesPopulations input
  blockScreenTillPress

menuGraphsBAD fn = do
  input <- getInputInfos fn
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
                 menuGraphsBAD fn
  blockScreenTillPress

menuTrajectories fn = do
  input <- getInputInfos fn
  genTrajectories input
  blockScreenTillPress

menuLifeTimes fn = do
  input <- getInputInfos fn
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
              menuLifeTimes fn
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
              menuLifeTimes fn
    otherwise -> do
                 putStrLn "\nI do not like you.\n"
                 menuLifeTimes fn

byeString="\nDynAnalyzer - by AcuZZio\n"

quitWithStyle fn = do
  setSGR [Reset]
  clearScreen
  putStrLn $ byeString ++ "\nThanks !!\n"
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

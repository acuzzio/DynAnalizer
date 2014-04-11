module Main where

import Control.Monad
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO

import CreateInfo2
import ParseInput
import GnuplotZ2
import CalculateDAT2

data Flag = Help
            | CreateInfo String
            | CheckInfo String
            | InputFile String
            deriving (Show, Eq)

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
            a <- doesFileExist fn
            case a of
              True     -> goIntoMenu fn
              False    -> writeInputTemplate fn
    CheckInfo folder   -> checkInfoFiles folder

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
  let content = "folder     = example_info_folder                 -- Here Info foldername\nchargeTrFragment = [1,2,3]                       -- Here list of Atom in charge transfer fraction\nccccList   = [5,4,6,7]                           -- Here the central dihedral\nbetaList   = [3,4,6,10]                          -- Here beta angle\nblaList    = [[(1,5),(4,6),(7,8)],[(4,5),(6,7)]] -- BLA list of single bonds, list of double bonds\nisomType   = Cis                                 -- Here Cis or Trans\nnRoot      = 2                                   -- This is the number of root in the system\n\n"
  putStrLn $ "\nTemplate input file: " ++ fn ++ " written.\n"
  putStrLn "Change it as you wish, then re-run this command !! \n"
  writeFile fn content

--MENUUUU

goIntoMenu fn = do
  let concatNums (i, (s, _)) = show i ++ " ) " ++ s
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
   ("Create graphics of Energies and Population", createGraphsEnePop),
   ("Quit", quitWithStyle)
    ]

--execute :: (Int,FilePath) -> IO ()
execute (n, fn) = let     
     doExec ((_, (_,f)):_) = f fn
     in doExec $ filter (\(i, _) -> i == n) choices

createGraphsEnePop fn = do
  input <- getInputInfos fn
  plotEnergiesPopulations input

quitWithStyle fn = do
  putStrLn "\nSee ya, mate !!\n"
  exitSuccess

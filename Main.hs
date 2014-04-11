module Main where

import Control.Monad
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO

import CreateInfo2

data Flag = Help
            | CreateInfo String
            | CheckInfo String
            | InputFile String
            deriving (Show, Eq)

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
     "this is to enter menu. It needs an input file. If it exists it will use it, if not, it will create a new template"
   ]

useMessage = putStrLn $ usageInfo "\nThis is how to use DynAnalyze:\n" options

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
              True     -> goIntoMenu
              False    -> writeInputTemplate fn
    CheckInfo folder   -> checkInfoFiles folder

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
  let content = "folder     = example_info_folder                 -- Here Info foldername\nchargeTrFragment = [1,2,3]                       -- Here list of Atom in charge transfer fraction\nccccList   = [5,4,6,7]                           -- Here the central dihedral\nbetaList   = [3,4,6,10]                          -- Here beta angle\nblaList    = [[(1,5),(4,6),(7,8)],[(4,5),(6,7)]] -- BLA list of single bonds, list of double bonds\nisomType   = Cis                                 -- Here Cis or Trans\nnRoot      = 2 :: Int                            -- This is the number of root in the system\n\n"
  putStrLn $ "\nFile: " ++ fn ++ " written.\n"
  putStrLn "Change it as you wish, then re-run this command !! \n"
  writeFile fn content

--MENUUUU

goIntoMenu :: IO()
goIntoMenu = do
  let concatNums (i, (s, _)) = show i ++ ".) " ++ s
  putStrLn . unlines $ map concatNums choices
  choice <- getLine
  case validate choice of
       Just n  -> execute . read $ choice
       Nothing -> putStrLn "Please try again"
  goIntoMenu

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]
choices = zip [1.. ] [
   ("Create", print "lol"),
   ("Quit", quitWithStyle)
    ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
     where doExec ((_, (_,f)):_) = f

quitWithStyle = do
  putStrLn "\nSee ya, mate !!\n"
  exitSuccess

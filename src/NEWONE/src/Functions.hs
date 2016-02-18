module Functions where

import Control.Concurrent.Async
import Data.List.Split
import Data.List
import Text.Printf
import System.Console.Terminal.Size
import System.Directory
import System.Process

import DataTypes

read2 :: String -> Double
read2 x = read x :: Double

readI :: String -> Int
readI x = read x :: Int

fromIntegral2 :: Int -> Double
fromIntegral2 x = fromIntegral x :: Double

writeF :: [[[String]]] -> String
writeF x  = intercalate "  \n"$ map unlines $ map (map unwords) x

avgListaListe :: [[Double]] -> [Double]
avgListaListe xss = map avg $ transpose xss

avg xs   = (sum xs)/(fromIntegral $ length xs)

devSt :: [Double] -> Double
devSt xs = let
  average = avg xs
  differences = map (\x -> x - average) xs
  l       = fromIntegral (length xs )
  in sqrt $ (sum(map (\x-> x**2) (differences)))/l

printZ x    = (printf "%.2f" x) :: String

printZ12 x    = (printf "%.10f" x) :: String

compress :: Eq a => [a] -> [a] 
compress = map head . group

findInd :: Plottable -> [Plottable] -> Int
findInd plo plos = let Just x = elemIndex plo plos
                     in x + 2

findIndM2 :: Plottable -> [Plottable] -> Int
findIndM2 plo plos = let Just x = elemIndex plo plos
                     in x 
-- parallel attempt

parallelProcFiles :: (a -> IO b) -> [a] -> IO ()
parallelProcFiles function outputs = do
       pids <- mapM (\x -> async $ function x) outputs
       mapM_ wait pids

correctFolderName :: String -> IO String
correctFolderName fn = if fn == "." 
   then do
        a  <- getCurrentDirectory
        let aa  = if (last a == '/') then init a else a     -- when the folder has '/' at the end
            -- aaa = reverse $ takeWhile (/= '/') $ reverse aa -- this was the bug of using Dynanalizer with the '.' from inside the project folder.
        return aa
   else do
        let aa  = if (last fn == '/') then init fn else fn
        return aa

--stringOnBox :: String -> String
--stringOnBox xs = let 
--      n = length xs
--      a = "  **" ++ (replicate n '*') ++ "**"
--      b = "  * " ++ (replicate n ' ') ++ " *"
--      c = "  * " ++        xs         ++ " *"
--      in intercalate "\n" [a,b,c,b,a]
--
stringOnBox :: String -> IO()
stringOnBox xs = do
      wdt <- terminalWidth
      let n         = length xs
          maxLeng   = fromIntegral $ wdt - 8
          reformat  = if n > maxLeng then reformatBanner maxLeng xs else [xs]
          len       = if n > maxLeng then maxLeng else n
          a   = "  **" ++ (replicate len '*') ++ "**"
          b   = "  * " ++ (replicate len ' ') ++ " *"
          c x = "  * " ++        x            ++ " *"
          d   = init $ unlines $ map c reformat
          total = intercalate "\n" [a,b,d,b,a]
      putStrLn total

lol="jfkahsdfkjlhsakdfhjsadfhjasdfkhsafhwsdfksajdfjasdfkhsaf hhsdf hasdfksa fh wsd fsadfjksadfjksafsjadfjklasjdfhasklflasdf as dfas kdfhasldfh"

reformatBanner maxLen str = let 
  pieces = chunksOf maxLen str
  in map (relength maxLen) pieces

relength mln str = if ln < mln then str ++ spaces else str
 where
  ln     = length str
  spaces = replicate (mln - ln) ' '

terminalWidth :: IO (Integer)
terminalWidth = do
      Just win <- size
      return $ width win

mosaic = map words . lines

unmosaic = unlines . map unwords

convFStoAU = 41.3414472
convAUtoFS =  0.0241888

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
  DihAnaPlot       _ -> 4


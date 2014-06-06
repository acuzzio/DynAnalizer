module Trajectories where

import System.ShQQ
import System.Directory
import System.Process  
import Data.List.Split

import CreateInfo
import IntCoor
import DataTypes
import ParseInput


genTrajectory :: Inputs -> FilePath -> IO()
genTrajectory input fn = do
  a <- rdInfoFile fn
  let folder = getfolder input
      dynname   = (takeWhile (/= '.') fn ) ++ ".md.xyz"
      atomT     = getAtomT a
      coords    = chunksOf atomN $ map (unwords . (map show) . runVec) $ getCoordinates a
      atomN     = getAtomN a
      header x  = (show atomN) ++ "\n \n" ++ x
      coordsAndType = zipWith (zipWith (\x y -> x ++ " " ++ y)) (repeat atomT) coords
      divided   = concat $ map (header . unlines) coordsAndType
      foldTraj  = "Trajectories"
  createDirectoryIfMissing True foldTraj
  writeFile dynname divided
  system $ "mv " ++ dynname ++ " " ++ foldTraj
  putStrLn $ dynname ++ ": Done"

genTrajectories :: Inputs -> IO()
genTrajectories input = do
   let folder = getfolder input
   outs <- readShell $ "ls INFO/*.info"
   let outputs = lines outs
   mapM_ (genTrajectory input) outputs
   putStrLn $ "\nTrajectories extracted into folder: " ++ folder ++ "/Trajectories\n"

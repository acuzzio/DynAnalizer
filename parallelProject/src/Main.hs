module Main where

import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import Data.Char (isDigit)
import Control.Concurrent.Async
import System.Process

import IntCoor
import CreateInfo


main = createInfoP

createInfoP = do
       outs <- readShell "ls */*.out"
       pwd  <- readShell "pwd"
       let outputs  = lines outs
           foldName = reverse $ takeWhile (\x-> x/='/') $ reverse $ head $ lines pwd -- dull again, but works like a charm
           chunks   = chunksOf 10 outputs
       sequence_ $ processFiles `fmap` chunks
       writeFile "shellforTakeATar.sh" $ shellZ foldName
       system "chmod 744 shellforTakeATar.sh"
       system "./shellforTakeATar.sh"
       system "rm shellforTakeATar.sh"


shellZ name = "mkdir temptemp\ncp */*.info temptemp/\ncd temptemp/\ntar -zcvf " ++ name ++ ".tgz *\nmv " ++ name ++ ".tgz ../\ncd ..\nrm -r temptemp"

processFiles :: [FilePath] -> IO ()
processFiles outputs = do
       pids <- mapM (\x -> async $ genInfoFile x) outputs
       mapM_ wait pids




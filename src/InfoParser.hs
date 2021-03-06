{-# LANGUAGE OverloadedStrings #-}

module InfoParser where

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
import Data.List.Split (chunksOf)
import System.ShQQ
import Text.Printf

import DataTypes
import IntCoor
import ParserFunctions
import Functions

--fn = "HopS.info"
--fn = "geom067.info"

labelInfo path = do
       infos <- readShell $ "ls " ++ path
       let outputs = lines infos
           chunks   = chunksOf 10 outputs
       sequence_ $ fmap (parallelProcFiles writeLabeledINFO) chunks

writeLabeledINFO :: FilePath -> IO()
writeLabeledINFO fn = do
  a <- readInfoFile fn
  let outputname = fn ++ "Labeled"
      formattedString = stringDynam a
  writeFile outputname formattedString


readInfoFile :: FilePath -> IO (Dynam)
readInfoFile fn = do
  a <- B.readFile fn
  case parseOnly parseInfo a of
       Left msg -> error "eeeeh InFo !@#$@#$"
       Right x  -> do 
                   let (nRoot,dt,aT,all) = x
                   return $ Dynam nRoot dt aT all

-- main file
parseInfo = do
  (nRoot,dt,aT) <- parseHeader
  let nAtom = length aT
  all <- many' (parseStep nRoot nAtom)
  return (nRoot,dt,aT,all)
  
parseHeader = do
  nRoot <- decimal <* anyLine'
  dt    <- double <* anyLine'
  aTbs  <- takeWhile1 isAlpha_ascii <* anyLine'
  let aT = B.unpack aTbs
  return (nRoot,dt,aT)

parseStep nRoot nAtom = do
  wf   <- count nRoot parseWF  -- still does not take anything
  geom <- parseVector nAtom
  chargeDipo  <- count nRoot parseChargeDipo
  let charge = map fst chargeDipo
      dipo   = map snd chargeDipo
      wfS    = map B.unpack wf
  enepop      <- parseEnePop
  grad        <- parseVector nAtom
  velo        <- parseVector nAtom
  [kin,tot]   <- parseKinTot 
  return $ Step wfS geom charge dipo enepop grad velo kin tot

parseKinTot = do
  kin <- double
  space
  tot <- double <* anyLine'
  return [kin,tot]

parseEnePop = double `sepBy` " " <* anyLine'

parseChargeDipo = do
  a <- parseCharge
  b <- parseDipo
  return (a,b)

parseCharge = double `sepBy` " " <* anyLine'

parseDipo = double `sepBy` " " <* anyLine'

parseWF = anyLine <* anyLine'
            
parseVector nAtom = do
  count nAtom parseTripletVec

parseTripletVec = do
  x <- double
  skipSpace
  y <- double
  skipSpace
  z <-double
  anyLine'
  return $ Vec [x,y,z]

--
-- 
-- PRETTY PRINTERS
--
--
--

stringDynam :: Dynam -> String
stringDynam dyna = let
  rootS          = "Root Numbers:\n" ++ (show (gtRootN dyna)) ++ "\n"
  dtS            = "Step size: \n" ++ (show (gtDT dyna)) ++ "\n"
  stringStepList = map (stringStep (gtAType dyna)) (gtStep dyna)  
  stepsS         = unlines $ enumerateStringWithLines (gtDT dyna) stringStepList
  in concat [rootS, dtS, stepsS]

stringStep :: String -> Step -> String
stringStep aTypes a = let
  wfS      = stringWF "---- Wave Function" (gtWf a)
  coordS   = stringVecDoubleLists aTypes "---- Coords" (gtCoor a)
  chargeS  = stringDoubleListList "---- Charges" (gtCharge a)
  dipoleS  = stringDoubleListList "---- Dipoles" (gtDipole a)
  enepopS  = stringDoubleList     "---- Energies Populations" (gtEnePop a)
  gradS    = stringVecDoubleLists aTypes "---- Gradient" (gtGrad a)
  veloS    = stringVecDoubleLists aTypes "---- Velocities" (gtVelo a)
  kinTotS  = stringKinTot (gtKin a) (gtTot a)
  in concat [wfS,coordS,chargeS,dipoleS,enepopS,gradS,veloS,kinTotS]

stringWF :: String -> [String] -> String
stringWF label wf = let
  transf = map transformOneWf wf
  labeled = enumerateString transf
  in label  ++ ":\n" ++  (concat labeled)
  
transformOneWf = unlines . map unwords . chunksOf 4 . words
  

stringVecDoubleLists :: String -> String -> [Vec Double] -> String
stringVecDoubleLists aTypes label vecDouble = let
  tripleList = map unwords $ map (map printBetter) $ map runVec vecDouble
  triplets = unlines $ zipWith (\x y -> (x : " ") ++ y ) aTypes tripleList
  in label ++ ":\n" ++ triplets

stringDoubleListList :: String -> [[Double]] -> String
stringDoubleListList label doubleLL = let
  triplets = unlines $ enumerateString $ map unwords $ map (map printBetter) doubleLL
  in label ++ ":\n" ++ triplets
  
enumerateString :: [String] -> [String]
enumerateString xs = let
  numbers = map labeled [1..]
  labeled x = "Root " ++ show x ++ ": \n"
  in zipWith (++) numbers xs

enumerateStringWithLines :: Double -> [String] -> [String]
enumerateStringWithLines dT xs = let
  numbers = map labeled [1..]
  labeled x = "--------------------- STEP " ++ show (floor x) ++ ": time = " ++ printBetter3 (x*dT-dT) ++ " a.u.  ---------------------\n"
  in zipWith (++) numbers xs

stringDoubleList :: String -> [Double] -> String
stringDoubleList label doubleL = let
  numbers = unwords $ map printBetter doubleL
  in label ++ ":\n" ++ numbers ++ "\n"

stringKinTot :: Double -> Double -> String
stringKinTot a b = "---- Kinetic Total:\n" ++ unwords [printBetter a, printBetter b]

printBetter x = (printf "%.5f" x) :: String
printBetter2 x = (printf "%.13f" x) :: String
printBetter3 x = (printf "%.2f" x) :: String


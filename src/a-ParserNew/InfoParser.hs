{-# LANGUAGE OverloadedStrings #-}

module InfoParser where

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import Control.Monad
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
import Data.List.Split (chunksOf)
import Text.Printf

import DataTypes
import IntCoor
import ParserFunctions

--fn = "HopS.info"
--fn = "geom067.info"

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
  labeled x = "--------------------- STEP " ++ show (floor x) ++ ": time = " ++ printBetter3 (x*dT-dT) ++ " ---------------------\n"
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

a=["1 2200 0.91945 0.84539 2 2ud0 0.35441 0.12561 3 2u0d 0.00597 0.00004 4 u2d0 -0.06595 0.00435 5 u20d -0.00834 0.00007 6 2020 -0.10509 0.01104 7 20ud -0.04889 0.00239 8 2002 -0.03085 0.00095 9 ud20 0.07180 0.00516 10 udud 0.04622 0.00214 11 ud02 0.00906 0.00008 12 0220 -0.04819 0.00232 13 02ud -0.01003 0.00010 14 0202 -0.01470 0.00022 15 uudd -0.00828 0.00007 16 u02d 0.00460 0.00002 17 u0d2 0.00022 0.00000 18 0u2d -0.00139 0.00000 19 0ud2 -0.00686 0.00005 20 0022 0.00322 0.00001 ","1 2200 0.33534 0.11245 2 2ud0 -0.87366 0.76328 3 2u0d 0.04346 0.00189 4 u2d0 -0.26337 0.06936 5 u20d -0.02114 0.00045 6 2020 -0.02677 0.00072 7 20ud -0.00056 0.00000 8 2002 -0.03753 0.00141 9 ud20 -0.20184 0.04074 10 udud -0.04047 0.00164 11 ud02 0.00789 0.00006 12 0220 0.07881 0.00621 13 02ud 0.00754 0.00006 14 0202 -0.00194 0.00000 15 uudd 0.00967 0.00009 16 u02d -0.02835 0.00080 17 u0d2 -0.01674 0.00028 18 0u2d 0.01500 0.00023 19 0ud2 0.01833 0.00034 20 0022 -0.00012 0.00000 ","1 2200 0.17937 0.03217 2 2ud0 -0.23870 0.05698 3 2u0d 0.05848 0.00342 4 u2d0 0.86171 0.74254 5 u20d -0.02350 0.00055 6 2020 0.35628 0.12693 7 20ud 0.06131 0.00376 8 2002 -0.01942 0.00038 9 ud20 0.11638 0.01354 10 udud 0.01796 0.00032 11 ud02 0.01826 0.00033 12 0220 -0.12657 0.01602 13 02ud -0.00913 0.00008 14 0202 -0.00750 0.00006 15 uudd 0.00889 0.00008 16 u02d 0.04026 0.00162 17 u0d2 0.03094 0.00096 18 0u2d -0.01556 0.00024 19 0ud2 -0.00198 0.00000 20 0022 -0.00163 0.00000 "]


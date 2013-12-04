import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative

atomnumbers = 17
atomtype = ["C","C","C","C","C","N","H","C","H","H","H","H","H","H","H","H","H"]


data Dinamica = Dinamica {
          getOutputNam   :: String,
          getEnergies    :: ([Double],[Double],[Double],[Double],[Double]),
          getCoordinates :: [Vec Double],
          getOscStr      :: [Double],
          getCharTran    :: [Double]
          } deriving Show

main = do
       main13

main13 = do
       values <- dinV
       label2 <- readShell "echo ${PWD##*/}"
       outs <- readShell "ls *.info"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let coordS = map (\x -> chunksOf atomnumbers $ getCoordinates x) a
           label   = init label2
           read2 x = read x :: Double
           ciao    = map transpose $ transposeDynV values 
           ciao2   = map (\x -> zipWith (:) (map show [1..]) x) ciao
           ciao3   = zipWith (\x y -> map (x:) y) (map show [1..]) ciao2
       putStrLn $ concat $ map unlines $ map (map unwords) ciao3
         

-- Prints inefficiently xyz files with coordinates that satisfy some conditions
-- condG
main12 = do
       values <- dinV
       label2 <- readShell "echo ${PWD##*/}"
       outs <- readShell "ls *.info"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let coordS = map (\x -> chunksOf atomnumbers $ getCoordinates x) a
           label   = init label2
           read2 x = read x :: Double
           ciao    = map transpose $ transposeDynV values 
           ciao2   = map (\x -> zipWith (:) (map show [1..]) x) ciao
           ciao3   = zipWith (\x y -> map (x:) y) (map show [1..]) ciao2
           condG x = x!!0 == "11"  
--           cond1 x = x!!7 == "S0"
--           cond2 x = read2 (x!!9) > 0.5
--           cond3 x = read2 (x!!3) < (-80) && read2 (x!!3) > (-100)
--           condG x = cond1 x && cond2 x && cond3 x
           filterZ = map (filter (condG)) ciao3
           count   = sum $ concat $ map (map (\x -> if x==[] then 0 else 1)) filterZ
           noEmpty = filter (/= []) filterZ
           getTupla (a:b:xs) = (read a :: Int,read b :: Int)
           tuplas  = concat $ map (map getTupla) noEmpty
       mapM_  (createXYZfromTuple coordS label) tuplas

createXYZfromTuple :: [[[Vec Double]]] -> String -> (Int, Int) -> IO ()
createXYZfromTuple coordS label (a,b) = do
     let name = "geom" ++ label ++ "_" ++ (printRightDigits a 2) ++ "_" ++ (printRightDigits b 3) ++ ".xyz"
         content = "17\n\n" ++ (unlines $ map unwords $ zipWith (:) atomtype (map (map (printf "%.13f")) $ map runVec $ coordS!!(pred a)!!(pred b)))
     writeFile name content


main11 = do
         outs <- readShell "ls *.info"
         let outputs = lines outs
         a <- mapM rdInfoFile outputs
         let coordS = map (\x -> chunksOf 17 $ getCoordinates x) a
             n=[(3,57),(5,80),(6,95),(32,67),(34,64),(43,73),(54,105),(93,92),(91,51),(99,107)] :: [(Int, Int)]
         mapM_ (\(x,y) -> writeFile ((show y) ++ ".xyz") x) $ zip (fmap (\(x,y) -> "17\n\n" ++ (unlines $ map unwords $ zipWith (:) atomtype (map (map (printf "%.13f")) $ map runVec $ coordS!!(pred x)!!(pred y)))) n) [0..]    

-- Creates info files from molcas output
main10 = do
       outs <- readShell "ls *.out"
       let outputs = lines outs
       mapM_ genInfoFile outputs

-- Creates averages divided by hop and isomerization (just remember that separateDins is one level more nested)
main9 = do
       outs <- readShell "ls *.info"
       label2 <- readShell "echo ${PWD##*/}"
       let label  = init label2
           outputs = lines outs
       a <- mapM rdInfoFile outputs
       let separateDins = separate a 
           ccccV        = map (map (corrFlip180 . diHedro cccc)) separateDins
           betaV        = map (map (corrFlip180 . corrStartingPointHoop . diHedro beta)) separateDins 
           tauV         = zipWith (zipWith (zipWith (\x y -> (x+y)*0.5))) betaV ccccV 
           deltaV       = zipWith (zipWith (zipWith (\x y -> (x-y)))) ccccV betaV
       writeFile ("averagecccc" ++ label) $ printWellAverages $ map avgListaListe ccccV
       writeFile ("averageBeta" ++ label) $ printWellAverages $ map avgListaListe betaV
       writeFile ("averageDeltaOp" ++ label) $ printWellAverages $ map avgListaListe deltaV
       writeFile ("averageTau" ++ label) $ printWellAverages $ map avgListaListe tauV

-- this main is because Massimo asked me the average of tau in the trajectories that do hop AND in S1
main91 = do
  outs <- readShell "ls *.info"
  label2 <- readShell "echo ${PWD##*/}"
  let label  = init label2
      outputs = lines outs
  a <- mapM rdInfoFile outputs
  let eliminateIfNoHop = separate2 a
      ccccV        = map (corrFlip180 . diHedro cccc) eliminateIfNoHop
      betaV        = map (corrFlip180 . corrStartingPointHoop . diHedro beta) eliminateIfNoHop
      tauV         = zipWith (zipWith (\x y -> (x+y)*0.5)) betaV ccccV
      isS1         = map rootDiscov eliminateIfNoHop
      tauAndlabel  = zipWith (zip) tauV isS1
      onlyS1       = map (filter (\x -> snd x == "S1")) tauAndlabel
      tauS1        = map (map fst) onlyS1
      mediatore    = avgListaListe tauS1
  writeFile "lol" $ unlines $ map show mediatore
  --return mediatore


-- Creates hop points.
main8 = do 
       outs <- readShell "ls *.info"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let dihe   = map (diHedro cccc) a
           justHop   = map justHopd a
           transformTupleZ (a,(b,c)) = (a,b,c)
           transformTupleZ2 (a,(b,c,d)) = (a,b,c,d)
           g      = zip dihe justHop
           tupla1 = map (\x -> zip (fst x) (snd x)) g
           tupla2 = map (zip [0..]) tupla1
           tupla3 = map (map transformTupleZ) tupla2
           tupla4 = map (map transformTupleZ2) $ zipWith (\x y -> zip (repeat x) y) [0..] tupla3
           fromTuplaToString (a,b,c,d) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d)
           fromTuplaToString' (a,b,c,d) = (show b) ++ " " ++ (show c) ++ " " ++ (show d)
           fitchero = unlines $ concat $ map (map fromTuplaToString) tupla4
       writeFile "out" fitchero


-- try to make gap and osc graphic
main7 = do
       outs <- readShell "ls *.info"
       label2 <- readShell "echo ${PWD##*/}"
       let label             = init label2
           outputs           = lines outs
       a <- mapM rdInfoFile outputs
       let isS1              = map rootDiscov a
           ccccV             = map (corrFlip180 . diHedro cccc) a
           read2 x           = read x :: Double
           cTV               = map getCharTran a
--           oscStrV           = map getOscStr a
--           diffV             = map energyDiff a
--           filediff          = make3dString'' ccccV diffV isS1
--           fileOsc           = make3dString'' ccccV oscStrV isS1
           fileCT            = make3dString'' ccccV cTV isS1
--       writeFile (label ++ "ccccAVERAGEGapS1S0") filediff
--       writeFile (label ++ "ccccOscStrength") fileOsc
       writeFile (label ++ "ccccChargeTran") fileCT

-- Creates file out for 3d graphic of dihedrals in TUTTE LE SALSE 
main6 = do 
       outs <- readShell "ls *.info"
       label <- readShell "echo ${PWD##*/}"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let isS1   = map rootDiscov a
           justHop= map justHopd a
           betaV  = map (corrFlip180 . corrStartingPointHoop . diHedro beta) a
           betaS  = make3dString betaV isS1
           betaH  = make3dString' betaV justHop
           ccccV  = map (corrFlip180 . diHedro cccc) a
           ccccS  = make3dString ccccV isS1
           ccccH  = make3dString' ccccV justHop
           tauV   = zipWith (zipWith (\x y -> (x+y)*0.5)) betaV ccccV 
           tauS   = make3dString tauV isS1
           tauH   = make3dString' tauV justHop
           deltaV = zipWith (zipWith (\x y -> (x-y))) ccccV betaV
           deltaS = make3dString deltaV isS1
           deltaH = make3dString' deltaV justHop
           blaV   = map (diHedro bla) a
           blaS   = make3dString blaV isS1
           blaH   = make3dString' blaV justHop
       writeTotalFiles (label ++ "betaTotal") betaS
       writeHopFiles (label ++ "betaHop") betaH
       writeTotalFiles (label ++ "ccccTotal") ccccS
       writeHopFiles (label ++ "ccccHop") ccccH
       writeTotalFiles (label ++ "tauTotal") tauS
       writeHopFiles (label ++ "tauHop") tauH
       writeTotalFiles (label ++ "delOPTotal") deltaS
       writeHopFiles (label ++ "delOPHop") deltaH
       writeTotalFiles (label ++ "Bla") blaS
       writeHopFiles (label ++ "Bla") blaH

-- those are the next request by massimo, the BOX with 2 variables and hopping points.
main5 = do 
       outs <- readShell "ls *.info"
       label <- readShell "echo ${PWD##*/}"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let isS1   = map rootDiscov a
           justHop= map justHopd a 
           betaV  = map (corrFlip180 . corrStartingPointHoop . diHedro beta) a
           ccccV  = map (corrFlip180 . diHedro cccc) a
           tauV   = zipWith (zipWith (\x y -> (x+y)*0.5)) betaV ccccV
           deltaV = zipWith (zipWith (\x y -> (x-y))) ccccV betaV
           blaV   = map (diHedro bla) a
       writeCombineFile (label ++ "ccccBla") $ combine3D ccccV blaV justHop isS1
       writeHopFiles (label ++ "ccccBla") $ combine3D ccccV blaV justHop isS1

-- last version of everything
main4 = do
       values <- dinV
       label2 <- readShell "echo ${PWD##*/}"
       let label  = init label2
           ciao   = map transpose $ transposeDynV values 
           ciao2  = map (\x -> zipWith (:) (map show [1..]) x) ciao
           ciao3  = zipWith (\x y -> map (x:) y) (map show [1..]) ciao2
--         ciao 3 is like this (all strings) [[[ 0 trajN,1 stepN,2 beta,3 cccc,4 Tau,5 DeltaOp,6 Bla,7 S1OrS2,8 HopYesNo,9 CT ]]]
           up     = map compress $ formatGnuplot3dFile ciao3 [1,3,6] (\x -> read2 (x!!9) > 0.5)
           upS0   = map compress $ formatGnuplot3dFile ciao3 [1,3,6] (\x -> read2 (x!!9) > 0.5 && x!!7=="S0")
           down   = map compress $ formatGnuplot3dFile ciao3 [1,3,6] (\x -> read2 (x!!9) < 0.5)
           downS0 = map compress $ formatGnuplot3dFile ciao3 [1,3,6] (\x -> read2 (x!!9) < 0.5 && x!!7=="S0")
           hopS0  = formatGnuplot3dFile ciao3 [1,3,6] (\x -> x!!8=="S0")
           hopS1  = formatGnuplot3dFile ciao3 [1,3,6] (\x -> x!!8=="S1")
           strai  = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) > 0.5 && x!!7=="S0")
           strai2 = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) < 0.5 && x!!7=="S0")
           writeSplot name dat = writeFile name $ unlines $ concat $ intersperse ["   "] $ map (filter (/= "embe")) $ map (map unwords) dat
           upper  = (-70.0)
           lower  = (-110.0)
           strai3 = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) > 0.5 && x!!7=="S0" && (read2 $ x!!3) < upper && (read2 $ x!!3) > lower)
           strai4 = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) < 0.5 && x!!7=="S0" && (read2 $ x!!3) < upper && (read2 $ x!!3) > lower)
           strai5 = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) > 0.5 && x!!7=="S0" && ((read2 $ x!!3) > upper || (read2 $ x!!3) < lower))
           strai6 = formatGnuplot3dFile ciao3 [0,1,3] (\x -> read2 (x!!9) < 0.5 && x!!7=="S0" && ((read2 $ x!!3) > upper || (read2 $ x!!3) < lower))
--       print $ map (filter (/=10)) $  map (map length) ciao3
--       print up
--       print $ map (map length) ciao3
--       putStrLn $ unlines $ concat $ map (map unwords) ciao3 
       writeSplot (label ++ "ccccBlaCTupS0") upS0
       writeSplot (label ++ "ccccBlaCTup") up
       writeSplot (label ++ "ccccBlaCTdownS0") downS0
       writeSplot (label ++ "ccccBlaCTdown") down
       writeSplot (label ++ "ccccBlaCTHopS0") hopS0
       writeSplot (label ++ "ccccBlaCTHopS1") hopS1
       writeSplot (label ++ "ccccTnCTup") strai
       writeSplot (label ++ "ccccTnCTdown") strai2
       writeSplot (label ++ "ccccTnCTupIn") strai3
       writeSplot (label ++ "ccccTnCTdownIn") strai4
       writeSplot (label ++ "ccccTnCTupOut") strai5
       writeSplot (label ++ "ccccTnCTdownOut") strai6


--new main where we want to know the points that are in a certain area of dihedrals
main3 = do
       values <- dinV
       label2 <- readShell "echo ${PWD##*/}"
       let label= init label2
           alldihedrals  = map (map (\x -> read x :: Double)) $ getCcccDih values
           allCT         = getCT values
           togheter      = zipWith (zip) alldihedrals allCT
           inside1       = map (map insideArea) togheter
           inside2       = map (map insideAreaAndUP) togheter
           transposethem = transpose inside1
           transposethem2= transpose inside2
           upperlimit    = -70.0
           downlimit     = -110.0
           insideArea (x,y)  = if x < upperlimit && x > downlimit then 1 else 0
           insideAreaAndUP(x,y)  = if x < upperlimit && x > downlimit && (read2 y > 0.5) then 1 else 0
           sumThem       = map sum transposethem
           sumThem2      = map sum transposethem2
           zipfswvalue   = zip3 [1..] sumThem sumThem2
           stringtransf (x,y,z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z)
           almostThere   = map stringtransf zipfswvalue
           unLinesThem   = unlines almostThere
       putStrLn unLinesThem

--this counts how long they stay there in media
main2 = do
       values <- dinV
       label2 <- readShell "echo ${PWD##*/}"
       let label             = init label2
           allDihedrals      = map (map (\x -> read x :: Double)) $ getCcccDih values
           allExorFond       = getS1OrS2 values
           togheter          = zipWith (zip) allDihedrals allExorFond
           upperlimit        = -70.0
           downlimit         = -110.0
           insideArea (x,y)  = if x < upperlimit && x > downlimit && y == "S0" then 1 else 0
           inside1           = map (map insideArea) togheter
           howMuchTime       = map sum inside1
           hoppingPoints     = map (\y -> fst $ safeHead $ filter (\x -> snd x == "S0") $ zip [1..] y) $ getHopYesNo values
           toPrint           = unlines $ zipWith (\x y -> x ++ " " ++ y) (map (show) hoppingPoints) (map show howMuchTime)
       putStrLn toPrint

formatGnuplot3dFile :: [[[String]]] -> [Int] -> ([String] -> Bool) -> [[[String]]] 
formatGnuplot3dFile a elem cond = map (map (\x -> if cond x then (map (x!!) elem) else [" "])) a

read2 x= read x :: Double

safeHead :: [(Int,String)] -> (Int,String)
safeHead []     = (0,"No")
safeHead (x:xs) = x

compress :: Eq a => [a] -> [a]
compress = map head . group

data DinamicV = DinamicV {
          getS1OrS2      :: [[String]]
         ,getHopYesNo    :: [[String]]
         ,getCT          :: [[String]]
         ,getBetaDih     :: [[String]]
         ,getCcccDih     :: [[String]]
         ,getTau         :: [[String]]
         ,getDeltaOp     :: [[String]]
         ,getBlaV        :: [[String]]
          } deriving Show

dinV :: IO DinamicV
dinV = do  
       outs <- readShell "ls *.info"
       let outputs = lines outs
       a <- mapM rdInfoFile outputs
       let isS1    = map rootDiscov a
           justHop = map justHopd a 
           cT      = map (map show) $ map (getCharTran) a
           betaV   = map (corrFlip180 . corrStartingPointHoop . diHedro beta) a
           ccccV   = map (corrFlip180 . diHedro cccc) a
           tauV    = zipWith (zipWith (\x y -> (x+y)*0.5)) betaV ccccV
           deltaV  = zipWith (zipWith (\x y -> (x-y))) ccccV betaV
           blaV    = map (diHedro bla) a
           strng x = map (map show) x
           dynV    = DinamicV isS1 justHop cT (strng betaV) (strng ccccV) (strng tauV) (strng deltaV) (strng blaV)
--       print $ zip (map length isS1) (map length isCTbig2)
       return dynV

transposeDynV dE = getZipList $ (\a b c d e f g h -> a:b:c:d:e:f:g:h:[]) <$> ZipList (getBetaDih dE) <*> ZipList (getCcccDih dE) <*> ZipList (getTau dE) <*> ZipList (getDeltaOp dE) <*> ZipList (getBlaV dE) <*> ZipList (getS1OrS2 dE) <*> ZipList (getHopYesNo dE) <*> ZipList (getCT dE)

combine3D :: [[Double]] -> [[Double]] -> [[String]] -> [[String]] -> String
combine3D uno due hopd isS1 = let
       transformTupleZ (a,(b,c)) = (a,b,c)
       transformTupleZ2 (a,(b,c,d)) = (a,b,c,d)
       transformTupleZ3 (a,(b,c,d,e)) = (a,b,c,d,e)
       fst'' (a,_,_,_) = a
       snd'' (_,b,_,_) = b
       trd'' (_,_,c,_) = c
       frt'' (_,_,_,d) = d
       g      = zip4 uno due hopd isS1
       tupla1 = map (\x -> zip4 (fst'' x) (snd'' x) (trd'' x) (frt'' x)) g
       tupla2 = map (map transformTupleZ3) $ map (zip [0..]) tupla1
       fromTuplaToString (a,b,c,d,e) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ d ++ " " ++ e
       in unlines $ concat $ intersperse ["S1\nS0"] $ map (map fromTuplaToString) tupla2


writeCombineFile :: FilePath -> String -> IO ()
writeCombineFile name cont = do 
               let back  = map words $ lines cont
                   remov = map (filter (/="S0") .filter (/="S1") . filter (/="no")) back
                   s0S  = map (filter (/="S0")) $ filter (\x -> elem "S0" x) remov
                   s1S  = map (filter (/="S1")) $ filter (\x -> elem "S1" x) remov
                   tOTS = map (filter (/="S0")) $ map (filter (/="S1")) $ remov
                   reformat xss = unlines $ map unwords xss
               writeFile (name ++ "S0") $ reformat s0S
               writeFile (name ++ "S1") $ reformat s1S
               writeFile (name ++ "TOT") $ reformat tOTS

writeTotalFiles name cont = do
               let back = map words $ lines cont
                   s0S  = map (filter (/="S0")) $ filter (\x -> elem "S0" x) back
                   s1S  = map (filter (/="S1")) $ filter (\x -> elem "S1" x) back
                   tOTS = map (filter (/="S0")) $ map (filter (/="S1")) $ back
                   reformat xss = unlines $ map unwords xss
               writeFile (name ++ "TotalS0") $ reformat s0S
               writeFile (name ++ "TotalS1") $ reformat s1S
               writeFile (name ++ "TotalTOT") $ reformat tOTS

writeHopFiles name cont = do
               let back = map words $ lines cont
                   s0S  = map (filter (/="S0")) $ filter (\x -> elem "S0" x) back
                   s1S  = map (filter (/="S1")) $ filter (\x -> elem "S1" x) back
                   reformat xss = unlines $ map unwords xss
               writeFile (name ++ "HopS0") $ reformat s0S
               writeFile (name ++ "HopS1") $ reformat s1S

make3dString'' :: (Show a) => [[Double]] -> [[Double]] -> [[a]] -> String
make3dString'' list stri str = let 
        transformTupleZ (a,(b,c)) = (a,b,c)
        transformTupleZ2 (a,(b,c,d)) = (a,b,c,d)
        transformTupleZ3 (a,(b,c,d,e)) = (a,b,c,d,e)
        g      = zip3 list stri str
        tupla1 = map (\x -> zip3 (fst' x) (snd' x) (trd' x)) g
        tupla2 = map (zip [0..]) tupla1
        tupla3 = map (map transformTupleZ2) tupla2
        tupla4 = map (map transformTupleZ3) $ zipWith (\x y -> zip (repeat x) y) [0..] tupla3
        fromTuplaToString (a,b,c,d,e) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e)
        in unlines $ concat $ intersperse ["   S1\n  S0"] $ map (map fromTuplaToString) tupla4

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

trd' :: (a,b,c) -> c
trd' (_,_,c) = c

make3dString' :: [[Double]] -> [[String]] -> String
make3dString' list stri = let transformTupleZ (a,(b,c)) = (a,b,c)
                              transformTupleZ2 (a,(b,c,d)) = (a,b,c,d)
                              g      = zip list stri
                              tupla1 = map (\x -> zip (fst x) (snd x)) g
                              tupla2 = map (zip [0..]) tupla1
                              tupla3 = map (map transformTupleZ) tupla2
                              tupla4 = map (map transformTupleZ2) $ zipWith (\x y -> zip (repeat x) y) [0..] tupla3
                              fromTuplaToString (a,b,c,d) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ d
                              in unlines $ concat $ intersperse ["   S1\n  S0"] $ map (map fromTuplaToString) tupla4


make3dString :: (Show a) => [[Double]] -> [[a]] -> String
make3dString list boolS = let transformTupleZ (a,(b,c)) = (a,b,c)
                              transformTupleZ2 (a,(b,c,d)) = (a,b,c,d)
                              g      = zip list boolS
                              tupla1 = map (\x -> zip (fst x) (snd x)) g
                              tupla2 = map (zip [0..]) tupla1
                              tupla3 = map (map transformTupleZ) tupla2
                              tupla4 = map (map transformTupleZ2) $ zipWith (\x y -> zip (repeat x) y) [0..] tupla3
                              fromTuplaToString (a,b,c,d) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d)
                              in unlines $ concat $ intersperse ["   S1\n S0"] $ map (map fromTuplaToString) tupla4

energyDiff :: Dinamica -> [Double]
energyDiff dyn = let (pop1,pop2,s0,s1,dynDyn) = getEnergies dyn
                 in zipWith (-) s1 s0

justHopd :: Dinamica -> [String]
justHopd dyn = let (pop1,pop2,s0,s1,dynDyn) = getEnergies dyn
                   changed (x:[]) = "no":[]
                   changed (x:xs) = if x==(head xs) then "no" : changed xs else if x == True then "S0" : changed xs else "S1" : changed xs
                   truefalse = zipWith (==) s1 dynDyn
                   cngTF     = changed truefalse
               in "no":"no":cngTF

rootDiscov :: Dinamica -> [String]
rootDiscov dyn = let (pop1,pop2,s0,s1,dynDyn) = getEnergies dyn
                     first = zipWith (\x y -> if x==y then "S1" else "S0") s1 dynDyn                      
                 in "S1":"S1":first

printWellAverages :: [[Double]] -> String
printWellAverages xs = unlines $ map unwords (map (map show) $ take 200 (transpose xs))

printWellList :: [Double] -> IO()
printWellList xs = putStrLn $ unlines $ map show xs

avgListaListe :: [[Double]] -> [Double]
avgListaListe xss = let avg xs = (sum xs)/(fromIntegral $ length xs) 
                    in map avg $ transpose xss

isomerizeYesNo :: Dinamica -> Bool
isomerizeYesNo dyn = let
  lastPoint      = last $ chunksOf atomnumbers $ getCoordinates dyn
  lastDihedral   = cccc lastPoint
  in if lastDihedral < 90.0 && lastDihedral > -90.0 then True else False

hoppedYesNo :: Dinamica -> Bool
hoppedYesNo dyn = let
  (_,_,_,s1Energies,dynEnergies) = getEnergies dyn
  in if s1Energies/=dynEnergies then True else False
  
separate :: [Dinamica] -> [[Dinamica]]
separate dyns = [filter (isomerizeYesNo) (filter hoppedYesNo dyns),filter (not . isomerizeYesNo) (filter hoppedYesNo dyns),filter (not . hoppedYesNo) dyns]

-- this separe dyn that hop and dyn that do not
separate2 :: [Dinamica] -> [Dinamica]
separate2 dyns = filter hoppedYesNo dyns

listar :: Double -> Double -> Int -> [Double]
listar max min res = let 
                    deno = fromIntegral res 
                    step  = (max-min)/(deno-1.0)
                    in take (res) $ iterate (+step) min 

tuplar :: [Double] -> [(Double,Double)]
tuplar (x:[]) = []
tuplar (x:xs) = (x,head xs) : tuplar xs

element :: (Double, Double) -> [Double] -> [Int]
element (min,max) [] = []
element (min,max) (x:xs) = if x > min && x < max 
                       then 1 : element (min,max) xs
                       else 0 : element (min,max) xs

howMany :: (Double, Double) -> [Double] -> Int 
howMany (min,max) [] = 0 
howMany (min,max) xs = sum $ element (min,max) xs

--rdDinamica :: String -> IO(Dinamica)
--rdDinamica fn = do
--    first  <- mapM (\a -> let val = show a
--                          in readShell $ "grep Gnuplot " ++ fn ++ " | awk '{print $" ++ val ++ "}' | awk 'NR % 200 == 0'") [2,3,4,5,6]
--    second <- readShell $ "grep -A20 '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
--    let enepop = map words first
--        floats = map (map (\x -> read x :: Double)) enepop
--        tupla  (a:b:c:d:e:[]) = (a,b,c,d,e)
--        tuplaEnerPop = tupla floats
--        coord1 = parseTriplet second
--    return $ Dinamica fn tuplaEnerPop coord1

rdInfoFile  :: String -> IO(Dinamica)
rdInfoFile fn = do
    cont <- readFile fn
    let (a:b:c:d:e:f:g:h:[]) = splitWhen (== "DIVISION") $ lines cont
        enepop = a:b:c:d:e:[]
        floats = map (map (\x -> read x :: Double)) enepop
        tupla  (a:b:c:d:e:[]) = (a,b,c,d,e)
        tuplaEnerPop = tupla floats
        coord1 = parseTriplet $ unlines f
        oscStr = map (\x-> read x :: Double) g
        charT  = map (\x-> read x :: Double) h
    return $ Dinamica fn tuplaEnerPop coord1 oscStr charT

genInfoFile :: String -> IO()
genInfoFile fn = do
    first  <- mapM (\a -> let val = show a
                          in readShell $ "grep Gnuplot " ++ fn ++ " | awk '{print $" ++ val ++ "}' | awk 'NR % 200 == 0'") [2,3,4,5,6]
    second <- readShell $ "grep -A20 '       Old Coordinates (time= ' " ++ fn ++ " | sed /--/d | sed /Coordinates/d | sed /Atom/d | awk '{print $3, $4, $5}'"
    third <- readShell $ "grep -A2 'Osc. strength.' " ++ fn ++ " | awk 'NR % 4 == 3' | awk '{print $3}'"
    fourth <- readShell $ "awk '/Mulliken population Analysis for root number: 1/ {flag=1;next} /Expectation values of various properties for root number:  1/ {flag=0} flag {print}' " ++ fn ++ " | grep N-E | sed s/N-E//" 
    let infoname = (takeWhile (/= '.') fn ) ++ ".info"
        div      = "DIVISION\n"
        first'   = concat $ intersperse div first
        fourth'  = unlines $ map show (calculateZ (novel $ distroccio (troccio fourth)))
        wholefile= first' ++ div ++ second ++ div ++ third ++ div ++ fourth'
    writeFile infoname wholefile

troccio = fmap (fmap (id) . words) . lines

distroccio xs = chunksOf atomnumbers (concat xs) 

novel g = map (map readDouble) g
     where readDouble = \x -> read x :: Double 

calculateZ g = map funct g
     where funct g = g!!0 + g!!1 + g!!2 + g!!7 + g!!9 + g!!12 + g!!13 + g!!14 + g!!15 + g!!16 


parseTriplet :: String -> [Vec Double]
parseTriplet = fmap (Vec .fmap (readDouble) . words) . lines
     where readDouble = \x -> 0.529177249 * (read x :: Double)

cccc,beta,hcch,bla :: [Vec Double] -> Double
cccc [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] =
     let res = dihedral [a2,a3,a4,a5]
     in res

hcch [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] =
     let res = dihedral [a8,a3,a4,a7]
     in res

beta [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] =
     let res = dihedral [a8,a3,a4,a7]
     in res

bla [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] =
     let doubles = (bond [a1,a2] + bond [a3,a4] + bond [a5,a6]) / 3
         singles = (bond [a2,a3] + bond [a4,a5]) / 2
         res = singles - doubles
     in res

corrDih :: [(Double,Double,Double)] -> [(Double,Double,Double)]
corrDih ((a,b,c):(d,e,f):[])  = (a,b,c) : (correct a d, correct b e, f) : []
corrDih ((a,b,c):(d,e,f):xs) = (a,b,c) : corrDih ((correct a d, correct b e, f):xs)

hoop :: [(Double,Double,Double)] -> [(Double,Double,Double)]
hoop [] = []
hoop ((a,b,c):xs) = (a,(a-b),c) : hoop xs

hoopToZero :: [(Double,Double,Double)] -> [(Double,Double,Double)]
hoopToZero [] = []
hoopToZero ((a,b,c):xs) = (a, (correct 0.0 b),c) : hoopToZero xs

corrHoop :: [(Double,Double,Double)] -> [(Double,Double,Double)]
corrHoop ((a,b,c):(d,e,f):[])  = (a,b,c) : (d, correct b e, f) : []
corrHoop ((a,b,c):(d,e,f):xs) = (a,b,c) : corrHoop ((d, correct b e, f):xs)

corrStartingPointHoop :: [Double] -> [Double]
corrStartingPointHoop [] = []
corrStartingPointHoop lst = if (head lst) > 0 then map (+(-360)) lst else lst

corrFlip180 :: [Double] -> [Double]
corrFlip180 [] = []
corrFlip180 lst = if (last lst) < (-300) then map (\x -> (-x)+(-360))lst else lst

correct :: Double -> Double -> Double
correct x y = let
              a = abs $ x - y
              b = abs $ x - (y + 360)
              c = abs $ x - (y - 360)
              f = minimum [a,b,c]
              in if a == f
                 then y else if b == f then (y + 360) else (y - 360) 

corrDihedro :: [Double] -> [Double]
corrDihedro (a:b:[]) = a : (correct a b) : []
corrDihedro (a:b:xs) = a : corrDihedro ((correct a b) : xs)

diHedro :: ([Vec Double] -> Double) -> Dinamica -> [Double]
diHedro fun dyn = let 
    dihedr  = map fun $ chunksOf atomnumbers $ getCoordinates dyn
    dihedro = corrDihedro dihedr 
    in dihedro

printRightDigits :: Int -> Int -> String
printRightDigits n nDigits =
     let
     leadingZeroes = concat $ take (nDigits - length(show n))(repeat "0")
     in leadingZeroes ++ show n


newtype Vec a = Vec {runVec :: [a]} deriving (Show, Read, Eq)

instance Functor Vec where
  fmap f (Vec v) = Vec $ f `fmap` v

instance Applicative Vec where
  pure x = Vec $ (repeat x)
  Vec fs <*> Vec xs = Vec $ Prelude.zipWith (\f x -> f x) fs xs

instance Num a => Num (Vec a) where
  (+)         = liftA2 (+) 
  (-)         = liftA2 (-) 
  (*)         = liftA2 (*) 
  abs         = liftA abs 
  signum      = liftA signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vec a) where
   (/)  = liftA2 (/) 
   recip v = recip <$> v
   fromRational = pure . fromRational

instance (Floating a) => Floating (Vec a) where
  pi = pure pi
  exp v = liftA exp v
  log v = liftA exp v
  sqrt v = liftA sqrt v
  (**)   = liftA2 (**)
  sin v =  sin <$> v
  cos v =  cos <$> v
  tan v =  tan <$> v
  asin v = asin <$> v
  acos v = acos <$> v
  atan v = atan <$> v
  sinh v = sinh <$> v
  cosh v = cosh <$> v
  tanh v = tanh <$> v
  asinh v = asinh <$> v
  acosh v = acosh <$> v
  atanh v = atanh <$> v

toVec :: [a] -> Vec a
toVec x = Vec x

-- ============> Methods <============

vecdot :: Num a => Vec a -> Vec a -> a
v1 `vecdot` v2 =  sum . runVec $ v1 * v2

vecCross :: Num a => Vec a -> Vec a -> Vec a
v1' `vecCross` v2' = let [[x,y,z],[u,v,t]]= fmap runVec [v1',v2']
                     in Vec $ [(y*t-v*z),(u*z-x*t),(x*v-u*y)]

vecscal :: Num a => a -> Vec a -> Vec a
x `vecscal` vec = (pure x) * vec

vecnorm :: Vec Double -> Double
vecnorm v =  sqrt  $  v `vecdot` v

-- =================> BOND, ANGLE AND DIHEDRAL <=========

dif2 :: [Vec Double] -> Double
dif2 [p1,p2,p3] = sqrt $ (p1 - p2) `vecdot` (p3 - p2)

bond :: [Vec Double] -> Double
bond [p1,p2] = vecnorm $ p1 - p2

angle :: [Vec Double] -> Double
angle [p1,p2,p3] = (180.0/pi*) . acos $ arg
  where arg = dif2 [p1,p2,p3] / ((p1 - p2) `vecdot` (p2 - p3))

dihedral :: [Vec Double] -> Double
dihedral [p1,p2,p3,p4] =
  let [xba,xca,xcb,xdb] = zipWith (-) [p2,p3,p3,p4] [p1,p1,p2,p2]
      [w1,w2] = zipWith vecCross [xba,xcb] [xca,xdb]
      [n1,n2] = map vecnorm [w1,w2]
      teta = (180.0/pi*) . acos $ ((w1 `vecdot` w2) / (n1*n2))
        in case 0.0 > (signum $ w2 `vecdot` xba) of
                True -> -teta
                False -> teta

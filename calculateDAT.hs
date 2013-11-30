import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import Data.Char (isDigit)

import IntCoor
import CreateInfo

data DinamicV = DinamicV {
          getDynN        :: [String]
         ,getStepN       :: [String]
         ,getS1OrS2      :: [String]
         ,getHopYesNo    :: [String]
         ,getCT          :: [String]
         ,getBetaDih     :: [String]
         ,getCcccDih     :: [String]
         ,getTau         :: [String]
         ,getDeltaOp     :: [String]
         ,getBlaV        :: [String]
          } deriving Show

main = do 
     outs <- readShell "ls *.info"
     let outputs = lines outs
     mapM_ createDATA outputs

--createData :: FilePath -> IO DinamicV
createDATA fn = do
    a             <- rdInfoFile fn
    let dataname  = (takeWhile (/= '.') fn ) ++ ".data"
        dynNum    = dropWhile (not. isDigit) $ takeWhile (/= '.') fn 
        dynN      = take (length isS1) $ repeat dynNum
        stepN     = take (length isS1) $ map show [1..200]
        isS1      = rootDiscov a
        atomN     = getAtomN a 
        justHop   = justHopd a
        cT        = getCharTran a
        betaV     = diHedro [8,3,4,7] atomN a 
        ccccV     = diHedro [2,3,4,5] atomN a
        tauV      = zipWith (\x y -> (x+y)*0.5) betaV ccccV
        deltaV    = zipWith (-) ccccV betaV
        blaV      = blaD atomN a
--        st        = map show
        prt       = map (\x -> printf "%.3f" x :: String)
        dynV      = DinamicV dynN stepN isS1 justHop (prt cT) (prt betaV) (prt ccccV) (prt tauV) (prt deltaV) (prt blaV)
    writeFile dataname $ printDinColumn dynV

--printDinColumn :: DinamicV -> String
printDinColumn dE = let trans = transposeDynV dE
                    in unlines $ map (" " ++) $ map unwords trans

transposeDynV dE = getZipList $ (\aaa aa a b c d e f g h -> aaa:aa:a:b:c:d:e:f:g:h:[]) <$> ZipList (getDynN dE) <*> ZipList (getStepN dE) <*> ZipList (getBetaDih dE) <*> ZipList (getCcccDih dE) <*> ZipList (getTau dE) <*> ZipList (getDeltaOp dE) <*> ZipList (getBlaV dE) <*> ZipList (getS1OrS2 dE) <*> ZipList (getHopYesNo dE) <*> ZipList (getCT dE)

energyDiff :: Dinamica -> [Double]
energyDiff dyn = let (pop1,pop2,s0,s1,dynDyn) = getEnergies dyn
                 in zipWith (-) s1 s0

justHopd :: Dinamica -> [String]
justHopd dyn = let (pop1,pop2,s0,s1,dynDyn) = getEnergies dyn
                   changed (x:[]) = "no":[]
                   changed (x:xs) = if x==(head xs) 
                                       then "no" : changed xs 
                                       else if x == True then "Y0" : changed xs else "Y1" : changed xs
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

hoppedYesNo :: Dinamica -> Bool
hoppedYesNo dyn = let
  (_,_,_,s1Energies,dynEnergies) = getEnergies dyn
  in if s1Energies/=dynEnergies then True else False
  
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

diHedro :: [Int] -> Int -> Dinamica -> [Double]
diHedro aL aN dyn = let 
    dihedr  = chunksOf aN $ getCoordinates dyn
    dihedrV = map dihedral $ map (\x -> map ( x !!) aL) dihedr
    in dihedrV

bonD :: [Int] -> Int -> Dinamica -> [Double]
bonD aL aN dyn = let
    bonds   = chunksOf aN $ getCoordinates dyn
    bondV   = map bond $ map (\x -> map ( x !!) aL) bonds
    in bondV

blaD :: Int -> Dinamica -> [Double]
blaD aN dyn = let
    blaDs = chunksOf aN $ getCoordinates dyn
    blaDV = map blaPSB3 blaDs
    in blaDV

blaPSB3 :: [Vec Double] -> Double
blaPSB3 list = let  -- atoms 1 2 + 3 4 + 5 6 / 3 
        doubles = (bond [list!!0,list!!1] + bond [list!!2,list!!3] + bond [list!!4,list!!5]) / 3
        singles = (bond [list!!1,list!!2] + bond [list!!3,list!!4]) / 2
        res = singles - doubles
        in res


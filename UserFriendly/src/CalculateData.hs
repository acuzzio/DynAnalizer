module CalculateData where

import Data.List
import Data.List.Split
import System.Process
import System.Directory
import System.ShQQ
import Text.Printf

import CreateInfo
import Data.Char (isDigit)
import DataTypes
import Functions
import IntCoor
import ParseInput

readerData :: IO AllTrajData
readerData = do
    outs            <- readShell $ "ls DATA/*.data"
    let outputs     = lines outs
    dataContent     <- mapM readFile outputs
    return $ map (map words) $ map lines dataContent

createDATAs input = do
   let listToPlot = getListToPlot input 
   outs <- readShell $ "ls INFO/*.info"
   let outputs    = lines outs
       dataFold   = "DATA" 
       folder = getfolder input
   createDirectoryIfMissing True dataFold
   mapM_ (createDATA input listToPlot) outputs
   system $ "mv INFO/*.data " ++ dataFold
   joinAllDATA folder
   putStrLn $ "\nYou can find data files into: " ++ folder ++ "/" ++ dataFold ++ " .\nI also wrote an all data file, have fun with Gnuplot !!\n"

createDATA input listToPlot fileInfo = do
   a             <- rdInfoFile fileInfo
   let dataname  = (takeWhile (/= '.') fileInfo ) ++ ".data"
       rlxR      = getStartRlxRt a
       dynNum    = reverse $ takeWhile (isDigit) $ reverse $ takeWhile (/= '.') fileInfo -- take Label
       outPut    = map (\x -> createPLOTDATA a input x) listToPlot
       dynN      = take (length $ outPut !! 0 ) $ repeat dynNum
       stepN     = take (length $ outPut !! 0 ) $ map show [1..]
       transpos0 = dynN : stepN : outPut
       transpos1 = map printWellSpacedColumn transpos0
       transpos  = transpose transpos1  -- I need to transpose them
   writeFile dataname $ unlines $ map unwords transpos

createPLOTDATA :: Dinamica -> Inputs -> Plottable -> [String] 
createPLOTDATA a input listToPlot = do
  let atomN      = getAtomN a
      geometries = chunksOf atomN $ getCoordinates a
      energies   = getEnergies a
      initialRlx = getStartRlxRt a
      mullChar   = chunksOf atomN $ getCharTran a
      ccccList   = getccccList input
      betaList   = getbetaList input
      blaList    = getblaList input
      cTFragment = getchargeTrFragment input
  case listToPlot of
    Cccc          -> corrDihedro2 input $ dihedro ccccList geometries 
    CcccCorrected -> corrDihedro3 $ dihedro ccccList geometries 
    Beta          -> corrDihedro2 input $ dihedro betaList geometries
    BetaCorrected -> corrDihedro3 $ dihedro betaList geometries
    Tau           -> let
                     cccc = map read2 $ corrDihedro3 $ dihedro ccccList geometries
                     beta = map read2 $ corrDihedro3 $ dihedro betaList geometries
                     tau  = zipWith (\x y -> (x+y)*0.5) cccc beta 
                     in prt tau
    Delta         -> let
                     cccc = map read2 $ corrDihedro3 $ dihedro ccccList geometries
                     beta = map read2 $ corrDihedro3 $ dihedro betaList geometries
                     delta= zipWith (-) cccc beta         
                     in prt delta
    Bla           -> blaD blaList geometries  
    Ct            -> calculateCT cTFragment mullChar
    Root          -> rootDiscov energies initialRlx
    Jump          -> justHopd energies initialRlx

dihedro :: [Int] -> [[Vec Double]] -> [String]
dihedro listAtom geometries = let
   aLIndex = map pred listAtom
   dihedrV = map dihedral $ map (\x -> map ( x !!) aLIndex) geometries
   in prt dihedrV

blaD :: [[(Int,Int)]] -> [[Vec Double]] -> [String]
blaD blaList geometries = let 
    blaDV = map (blaPSB3 blaList) geometries
    in prt blaDV

blaPSB3 :: [[(Int,Int)]] -> [Vec Double] -> Double
blaPSB3 blaList geometry = let 
    (a:b:c:[])      = blaList !! 0 
    (d:e:[])        = blaList !! 1
    blaBond (fS,sN) = bond [geometry!!(pred fS), geometry!!(pred sN)]
    doubles         = (blaBond a + blaBond b + blaBond c) / 3 
    singles         = (blaBond d + blaBond e) / 2 
    res             = singles - doubles
    in res

calculateCT :: [Int] -> [[Double]] -> [String]
calculateCT cTFragment charges = let
    cTFragmentI         = map pred cTFragment
    sumUp4CT x          = sum $ map (x!!) cTFragmentI
    cTvalues            = map sumUp4CT charges
    in prt cTvalues

corrDihedro3 :: [String] -> [String]
corrDihedro3 dihedListS = let 
   dihedList = map read2 dihedListS 
   firstDih  = head dihedList
   corr  x   = corrDihedro $ corrDihedro x
   in case isDihCloser firstDih 0 180 of
           0   -> let result = corr dihedList
                  in prt result
           180 -> let result = if firstDih < 0 then corr dihedList else corr $ map (\x -> x-360.0) dihedList
                  in prt result

corrDihedro2 :: Inputs -> [String] -> [String] 
corrDihedro2 input xl = let 
   doubles     = map read2 xl
   upperShift  = fst $ getUpperAndIsomCond $ getisomType input
   shiftDown x = if x > upperShift then x-360.0 else x
   in prt $ map shiftDown doubles

-- is dihedral angle (float :: Double) closer to (first :: Int) or (second :: Int) ? 
isDihCloser :: Double -> Int -> Int -> Int
isDihCloser float first second = let
   integ      = floor float :: Int
   a          = integ - 179
   b          = integ + 180
   downward y = if y > 180 then y - 360 else y
   upward   y = if y <= (-180) then y + 360 else y
   posOrNeg   = if signum float == 1 then map downward [a..b] else map upward [a..b]
   Just fir   = elemIndex first posOrNeg
   Just sec   = elemIndex second posOrNeg
   one        = abs (179 - fir) -- integ will always be at index 179 in this array
   two        = abs (179 - sec)
   in if one < two then first else second

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

-- to explain getI : imagine a line like this (ene ene pop pop dyn)  0.54 0.57 0.01 0.99 0.57, we want this function to return the index of 0.57 (1), that is the root that has the same value as dyn value. To do this we do a ZIP, we dropWhile the value is different and we return the INDEX (snd).
rootDiscov :: [[Double]] -> Int -> [String]
rootDiscov energy rlxInit = let
    rootS          = div (length energy - 1) 2
    [popu,ene,dyn] = chunksOf rootS energy
    startingRootS  = "S" ++ (show $ pred rlxInit) -- first two steps... no tully no party
    getI ls nu     = snd $ head $ dropWhile (\x-> fst x /= nu) $ zip ls [0..]
    rightRootI     = zipWith getI (transpose ene) (head dyn) -- dyn is still in a list because of ChunksOf
    rightRootS     = map (\x -> "S" ++ (show x)) rightRootI
    in startingRootS:startingRootS:rightRootS

justHopd :: [[Double]] -> Int -> [String]
justHopd energy rlxInit = let
    state a         = dropWhile ('S'==) a
    listaRoot       = rootDiscov energy rlxInit
    changE (x:[])   = "no":[]
    changE (x:xs)   = if x == (head xs)
                        then "no" : changE xs
                        else ((state x) ++  (state (head xs))) : changE xs
    cngTF           = changE listaRoot
    in cngTF

printWellSpacedColumn xs = let 
    matchLength n str = if length str == n then str else matchLength n $ " " ++ str 
    maxLength = maximum $ map length xs
    in map (matchLength maxLength) xs

prt :: [Double] -> [String]
prt doubles = map (\x -> printf "%.3f" x :: String) doubles

joinAllDATA :: FilePath -> IO ()
joinAllDATA folder = do  
    outs <- readShell $ "ls DATA/*.data"
    let outputs = lines outs
    dataContent  <- mapM readFile outputs
    writeFile (folder ++ "-all.data") $ intercalate "  \n" dataContent



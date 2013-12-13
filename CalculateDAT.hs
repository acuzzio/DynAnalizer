import System.ShQQ
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import Data.Char (isDigit)

import IntCoor
import CreateInfo
import Inputs

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
     outs <- readShell $ "ls " ++ folder ++ "/*.info"
     let outputs = lines outs
     mapM_ (createDATA betaList ccccList) outputs

--createData :: FilePath -> IO DinamicV
createDATA betaList ccccList fn = do
    a             <- rdInfoFile fn
    let dataname  = (takeWhile (/= '.') fn ) ++ ".data"
        rlxR      = getStartRlxRt a
        dynNum    = reverse $ takeWhile (isDigit) $ reverse $ takeWhile (/= '.') fn
        isS1      = rootDiscov a rlxR
        dynN      = take (length isS1) $ repeat dynNum
        stepN     = take (length isS1) $ map show [1..]
        atomN     = getAtomN a 
        justHop   = justHopd a rlxR
        cT        = calculateCT atomN $ getCharTran a
        betaV     = diHedro betaList atomN a 
        ccccV     = diHedro ccccList atomN a
        tauV      = zipWith (\x y -> (x+y)*0.5) betaV ccccV
        deltaV    = zipWith (-) ccccV betaV
        blaV      = blaD atomN a
--        st        = map show
        prt       = map (\x -> printf "%.3f" x :: String)
        pr        = printWellSpacedColumn . prt
        pw        = printWellSpacedColumn
        dynV      = DinamicV (pw dynN) (pw stepN) isS1 justHop (pr cT) (pr betaV) (pr ccccV) (pr tauV) (pr deltaV) (pr blaV)
    writeFile dataname $ printDinColumn dynV

printWellSpacedColumn xs = let 
    matchLength n str = if length str == n then str else matchLength n $ " " ++ str
    maxLength = maximum $ map length xs
    in map (matchLength maxLength) xs


--printDinColumn :: DinamicV -> String
printDinColumn dE = let trans = transposeDynV dE
                    in unlines $ map (" " ++) $ map unwords trans

transposeDynV dE = getZipList $ (\aaa aa a b c d e f g h -> aaa:aa:a:b:c:d:e:f:g:h:[]) <$> ZipList (getDynN dE) <*> ZipList (getStepN dE) <*> ZipList (getBetaDih dE) <*> ZipList (getCcccDih dE) <*> ZipList (getTau dE) <*> ZipList (getDeltaOp dE) <*> ZipList (getBlaV dE) <*> ZipList (getCT dE) <*> ZipList (getS1OrS2 dE) <*> ZipList (getHopYesNo dE) 

joinAllDATA :: IO ()
joinAllDATA = do 
    outs <- readShell $ "ls " ++ folder ++ "/*.data"
    let outputs = lines outs
    dataContent  <- mapM readFile outputs
    writeFile (folder ++ "-all.data") $ intercalate "  \n" dataContent  

readData :: FilePath -> IO [[String]]
readData fn = do 
        dataContent  <- readFile fn
        return $ map words $ lines dataContent

calculateCT :: Int -> [Double] -> [Double]
calculateCT a xs = let 
     dividedGeometries   = chunksOf a xs
     chargeTrFragmentI   = map pred chargeTrFragment
     sumUp4CT x          = sum $ map (x!!) chargeTrFragmentI
     in map sumUp4CT dividedGeometries

justHopd :: Dinamica -> Int -> [String]
justHopd dynam rlxD = let 
    state a         = dropWhile ('S'==) a 
    listaRoot       = rootDiscov dynam rlxD
    changE (x:[])   = "no":[]
    changE (x:xs)   = if x == (head xs) 
                        then "no" : changE xs 
                        else ((state x) ++  (state (head xs))) : changE xs
    cngTF           = changE listaRoot
    in cngTF

rootDiscov :: Dinamica -> Int -> [String]
rootDiscov dynam rlxD = let 
     energy         = getEnergies dynam
     rootS          = div (length energy - 1) 2
     [popu,ene,dyn] = chunksOf rootS energy
     startingRootS  = "S" ++ (show $ pred rlxD) -- first two steps... no tully no party
     getI ls nu     = snd $ head $ dropWhile (\x-> fst x /= nu) $ zip ls [0..]
     rightRootI     = zipWith getI (transpose ene) (head dyn)
     rightRootS     = map (\x -> "S" ++ (show x)) rightRootI 
     in startingRootS:startingRootS:rightRootS

printWellAverages :: [[Double]] -> String
printWellAverages xs = unlines $ map unwords (map (map show) $ take 200 (transpose xs))

printWellList :: [Double] -> IO()
printWellList xs = putStrLn $ unlines $ map show xs

avgListaListe :: [[Double]] -> [Double]
avgListaListe xss = let avg xs = (sum xs)/(fromIntegral $ length xs) 
                    in map avg $ transpose xss

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

corrDihedro2 :: [Double] -> [Double]
corrDihedro2 xl = let shiftDown x = if x > upperLimit then x-360.0 else x
                  in map shiftDown xl

diHedro :: [Int] -> Int -> Dinamica -> [Double]
diHedro aL aN dyn = let 
    aLIndex = map pred aL
    dihedr  = chunksOf aN $ getCoordinates dyn
    dihedrV = map dihedral $ map (\x -> map ( x !!) aLIndex) dihedr
    in corrDihedro2 dihedrV

bonD :: [Int] -> Int -> Dinamica -> [Double]
bonD aL aN dyn = let
    bonds   = chunksOf aN $ getCoordinates dyn
    bondV   = map bond $ map (\x -> map ( x !!) aL) bonds
    in bondV

blaD :: Int -> Dinamica -> [Double]
blaD aN dyn = let
    blaDs = chunksOf aN $ getCoordinates dyn
    blaDV = map (blaPSB3 blaList) blaDs
    in blaDV

blaPSB3 :: [[(Int,Int)]] -> [Vec Double] -> Double
blaPSB3 blaList list = let
        (a:b:c:[])      = blaList !! 0 
        (d:e:[])        = blaList !! 1
        blaBond (fS,sN) = bond [list!!(pred fS),list!!(pred sN)]
        doubles         = (blaBond a + blaBond b + blaBond c) / 3
        singles         = (blaBond d + blaBond e) / 2
        res             = singles - doubles
        in res

reportMassimo :: IO()
reportMassimo = do
    outs            <- readShell $ "ls " ++ folder ++ "/*.data"
    let outputs     = lines outs
    dataContent     <- mapM readFile outputs
    let stringZ     = map (map words) $ map lines dataContent
        checkLN     = zip [0..] $ map length stringZ
        filtered    = unwords $ map (show . fst) $ filter (\x -> snd x < 200) checkLN
        messaGe     = if filtered == "" then "Everything OK" else "Check out short trajectories: " ++ filtered
    putStrLn messaGe
    let getCCCC     = map (map (\a -> [a!!0,a!!1,a!!3])) stringZ
        nRootI      = nRoot - 1
        allJumps    = [(show x) ++ (show y) | x <- [0.. nRootI], y <- [0.. nRootI], x/=y] 
        getHOP root = map (map (\a -> [a!!0,a!!1,a!!3])) $ filter (\x -> x /= []) $ map (filter (\x-> x!!9 == root)) stringZ
        getHOPs     = map getHOP $ allJumps
        writeF x    = intercalate "  \n"$ map unlines $ map (map unwords) x
        avgDihedral = map (map (\a -> read (a!!3) :: Double)) $ map (filter (\x -> x!!8=="S1")) $ transpose stringZ
        avg xs   = (sum xs)/(fromIntegral $ length xs)
        avgZip   = zip [1..] $ map avg avgDihedral -- steps starts from 1
        form (a,b) = [show a,show b]
        ccccAVG  = unlines $ take 100 $ map unwords $ map form avgZip
    writeFile "CCCC" $ writeF getCCCC
    writeFile "CCCCS1AVG" ccccAVG
    mapM_ (\x -> writeFile ("CCCCHOP" ++ fst x) $ writeF (getHOPs !! snd x)) $ zip allJumps [0..]
    let hopOrNot    = map (all (\x -> x /= "10")) $ map (map (\x-> x!!9)) stringZ 
        isomYorN xs = map (map (\a -> read (a!!3) :: Double)) $ map (stringZ !!) xs
        counter x   = if x > -90.0 then 1 else 0
        whoNotHop   = map fst $ filter (\x-> snd x == True) $ zip [0..] hopOrNot
        notHopIsoC  = sum $ map counter $ map last $ isomYorN whoNotHop
        notHopnIsoC = (length whoNotHop) - notHopIsoC
        whoHop      = map fst $ filter (\x-> snd x == False) $ zip [0..] hopOrNot
        whoHopAndIs = map fst $ filter (\x-> snd x > -90.0) $ zip whoHop $ map last $ isomYorN whoHop
        whoHopAndNo = map fst $ filter (\x-> snd x < -90.0) $ zip whoHop $ map last $ isomYorN whoHop
        hopIsoC     = sum $ map counter $ map last $ isomYorN whoHop     
        hopNIsoC    = (length whoHop) - hopIsoC
        aveRAGES    = map (\x -> averageSublist stringZ x 3 200) [whoNotHop,whoHopAndIs,whoHopAndNo] -- 3 is cccc, 200 is first 200 substeps
        printZ x    = (printf "%.2f" x) :: String
    writeFile "CCCCAVERAGES" $ unlines $ map unwords $ map (map printZ) $ transpose aveRAGES
    putStrLn $ "Hop and Iso -> " ++ (show hopIsoC)
    putStrLn $ "Hop not Iso -> " ++ (show hopNIsoC)
    putStrLn $ "NoHop and Iso -> " ++ (show notHopIsoC)
    putStrLn $ "NoHop not Iso -> " ++ (show notHopnIsoC)
    let total = hopNIsoC + hopIsoC + notHopnIsoC + notHopIsoC
    putStrLn $ "Total -> " ++ (show total)
    let rateHOP = (fromIntegral (hopIsoC * 100) / (fromIntegral (hopIsoC+hopNIsoC))) :: Double
    putStrLn $ "only Hopped Iso/notIso -> " ++ (printZ rateHOP) ++ "%"
    let rateTOT = (fromIntegral (hopIsoC * 100) / fromIntegral (total)) :: Double
    putStrLn $ "Total Iso/notIso -> " ++ (printZ rateTOT) ++ "%"

averageSublist :: [[[String]]] -> [Int] -> Int -> Int -> [Double]
averageSublist stringOne trajxs index thres = let
        rightTrajectories = map (stringOne !!) trajxs
        rightValue        = map (map (\x -> x!!index)) rightTrajectories
        rightFloat        = map (map (\x -> read x :: Double)) rightValue
        avg xs   = (sum xs)/(fromIntegral $ length xs)
        in map avg $ take thres $ transpose rightFloat



 

--import Text.Printf
import System.ShQQ
--import Text.Printf
--import Data.List.Split
import Data.List
--import Control.Applicative
--import Data.Char (isDigit)
--
--
import CalculateDAT
import IntCoor
import CreateInfo
import Inputs
import Mapped
import Functions
import GnuplotZ


reportMassimo :: IO()
reportMassimo = do
    stringZ <- readerData 
    let checkLN     = zip [0..] $ map length stringZ
        filtered    = unwords $ map (show . fst) $ filter (\x -> snd x < 200) checkLN
        messaGe     = if filtered == "" then "Everything OK" else "Check out short trajectories: " ++ filtered
    putStrLn messaGe
    let avgDihedral = map (map (\a -> read (a!!3) :: Double)) $ map (filter (\x -> x!!8=="S1")) $ transpose stringZ
        avg xs   = (sum xs)/(fromIntegral $ length xs)
        avgZip   = zip [1..] $ map avg avgDihedral -- steps starts from 1
        form (a,b) = [show a,show b]
        ccccAVG  = unlines $ take 100 $ map unwords $ map form avgZip
    writeFile "CCCC" $ writeF stringZ
    writeFile "CCCCS1AVG" ccccAVG
    let hopOrNot    = map (all (\x -> x /= "10")) $ map (map (\x-> x!!9)) stringZ 
        isomYorN xs = map (map (\a -> read (a!!3) :: Double)) $ map (stringZ !!) xs
        counter x   = if isomCond x then 1 else 0
        whoNotHop   = map fst $ filter (\x-> snd x == True) $ zip [0..] hopOrNot
        notHopIsoC  = sum $ map counter $ map last $ isomYorN whoNotHop
        notHopnIsoC = (length whoNotHop) - notHopIsoC
        whoHop      = map fst $ filter (\x-> snd x == False) $ zip [0..] hopOrNot
        whoHopAndIs = map fst $ filter (\x-> isomCond $ snd x) $ zip whoHop $ map last $ isomYorN whoHop
        whoHopAndNo = map fst $ filter (\x-> not . isomCond $ snd x ) $ zip whoHop $ map last $ isomYorN whoHop
        hopIsoC     = sum $ map counter $ map last $ isomYorN whoHop     
        hopNIsoC    = (length whoHop) - hopIsoC
        aveRAGES    = map (\x -> averageSublist stringZ x 3 200) [whoNotHop,whoHopAndIs,whoHopAndNo] -- 3 is cccc, 200 is first 200 substeps
    writeFile "CCCCAVERAGES" $ unlines $ map unwords $ map (map printZ) $ transpose aveRAGES
    putStrLn $ "Hop and Iso -> " ++ (show hopIsoC)
    putStrLn $ "Hop not Iso -> " ++ (show hopNIsoC)
    putStrLn $ "NoHop and Iso -> " ++ (show notHopIsoC)
    putStrLn $ "NoHop not Iso -> " ++ (show notHopnIsoC)
    let total = hopNIsoC + hopIsoC + notHopnIsoC + notHopIsoC
    putStrLn $ "Total -> " ++ (show total)
    let rateHOP = (fromIntegral (hopIsoC * 100) / (fromIntegral (hopIsoC+hopNIsoC))) :: Double
    putStrLn $ "only Hopped Iso/notIso -> " ++ (printZ rateHOP) ++ "%"
    let rateNOH = (fromIntegral (notHopIsoC * 100) / (fromIntegral (notHopIsoC+notHopnIsoC))) :: Double 
    putStrLn $ "only NON Hopped Iso/notIso -> " ++ (printZ rateNOH) ++ "%"
    let rateTOT = (fromIntegral (hopIsoC * 100) / fromIntegral (total)) :: Double
    putStrLn $ "Total Iso/notIso -> " ++ (printZ rateTOT) ++ "%"
    hopS
    mapped stringZ 3 "CCCCDensity"
    chargeTmap stringZ "TOT" [0.4,0.5,0.6]
    let sZeroOnly = map (filter (\x -> x!!8 == "S0")) stringZ
    chargeTmap sZeroOnly "S0" [0.4,0.5,0.6]

calculateLifeTime root = do
    tupla <- averageLifetime root
    let result = barbattiFitting tupla
    print result

-- this is the most accurate one, that calculates the medium popoulation to be used with root INDEX, 1 or 2
averageLifetime state = do
    outs            <- readShell $ "ls " ++ folder ++ "/*.info"
    let outputs     = lines outs
    stinGZ          <- mapM rdInfoFile outputs
    let energies    = map getEnergies stinGZ
        rightstate  = state -1
        rightArray  = map (\x -> x!!rightstate) energies
        rightAverag = avgListaListe rightArray
        tupla       = zip (map fromIntegral2 [1..]) rightAverag
    return tupla

-- to be used with "S1" or "S2"
averageDynIntoState state = do
    stringZ <- readerData
    let transposed = transpose stringZ 
    let rightOnes  = map (filter (\x -> x!!8 == state)) transposed
        counter    = map length rightOnes
--    putStrLn $ unlines $ map show counter
        toDoubl    = map fromIntegral2 counter 
    return $ zip (map fromIntegral2 [1..]) toDoubl

-- equation 9 and 10 http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
--exponentialFitting :: [(Double, Double)] -> [Double]
exponentialFitting tuplas = let 
    xs = map fst tuplas
    ys = map snd tuplas
    one     = sum $ zipWith (\x y -> x**2 * y) xs ys
    two     = sum $ map (\y -> y * (log y)) ys
    three   = sum $ zipWith (\x y -> x * y) xs ys
    four    = sum $ zipWith (\x y -> x*y*(log y)) xs ys
    five    = sum ys
    fitA    = ((one * two) - (three * four))/((five*one)-(three**2))
    fitB    = ((five * four) - (three * two)) /((five*one)-(three**2))
    in [exp (fitA),fitB]

-- solve normal exponents regression first, then t1 and t2 from this paper (page 2794 - Nonadiabatic Photodynamics of a retinal model in polar and nonpolar environment, Ruckenbauer) are as in the code, where a and b are the same as this general formula - > y = A exp (Bx) :
barbattiFitting tuplas = let
    [a,b] = exponentialFitting tuplas
    t1    = -(( log a ) / b )
    t2    = (-1)/b
    tau   = t1 + t2
    in tau

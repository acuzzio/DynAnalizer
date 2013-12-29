--import Text.Printf
--import System.ShQQ
--import Text.Printf
--import Data.List.Split
import Data.List
--import Control.Applicative
--import Data.Char (isDigit)
--
--
import CalculateDAT
--import IntCoor
--import CreateInfo
import Inputs
import Mapped
import Functions
--import GnuplotZ


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

averageLifetime state = do
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




a = [(1.0,100.0),(2.0,100.0),(3.0,100.0),(4.0,98.0),(5.0,96.0),(6.0,93.0),(7.0,93.0),(8.0,93.0),(9.0,91.0),(10.0,90.0),(11.0,90.0),(12.0,91.0),(13.0,92.0),(14.0,92.0),(15.0,92.0),(16.0,92.0),(17.0,92.0),(18.0,94.0),(19.0,94.0),(20.0,95.0),(21.0,95.0),(22.0,96.0),(23.0,97.0),(24.0,97.0),(25.0,96.0),(26.0,96.0),(27.0,96.0),(28.0,96.0),(29.0,96.0),(30.0,95.0),(31.0,95.0),(32.0,94.0),(33.0,93.0),(34.0,91.0),(35.0,91.0),(36.0,91.0),(37.0,90.0),(38.0,90.0),(39.0,90.0),(40.0,90.0),(41.0,90.0),(42.0,89.0),(43.0,89.0),(44.0,87.0),(45.0,87.0),(46.0,87.0),(47.0,85.0),(48.0,84.0),(49.0,83.0),(50.0,84.0),(51.0,84.0),(52.0,84.0),(53.0,84.0),(54.0,84.0),(55.0,84.0),(56.0,84.0),(57.0,81.0),(58.0,80.0),(59.0,77.0),(60.0,78.0),(61.0,76.0),(62.0,73.0),(63.0,73.0),(64.0,73.0),(65.0,72.0),(66.0,71.0),(67.0,70.0),(68.0,68.0),(69.0,68.0),(70.0,68.0),(71.0,66.0),(72.0,64.0),(73.0,64.0),(74.0,60.0),(75.0,60.0),(76.0,59.0),(77.0,57.0),(78.0,56.0),(79.0,54.0),(80.0,53.0),(81.0,53.0),(82.0,53.0),(83.0,51.0),(84.0,50.0),(85.0,50.0),(86.0,50.0),(87.0,51.0),(88.0,51.0),(89.0,51.0),(90.0,50.0),(91.0,50.0),(92.0,49.0),(93.0,49.0),(94.0,48.0),(95.0,47.0),(96.0,47.0),(97.0,47.0),(98.0,47.0),(99.0,47.0),(100.0,45.0),(101.0,43.0),(102.0,44.0),(103.0,44.0),(104.0,44.0),(105.0,43.0),(106.0,42.0),(107.0,41.0),(108.0,41.0),(109.0,40.0),(110.0,40.0),(111.0,40.0),(112.0,40.0),(113.0,39.0),(114.0,38.0),(115.0,38.0),(116.0,38.0),(117.0,37.0),(118.0,37.0),(119.0,36.0),(120.0,35.0),(121.0,35.0),(122.0,35.0),(123.0,34.0),(124.0,34.0),(125.0,33.0),(126.0,33.0),(127.0,32.0),(128.0,31.0),(129.0,32.0),(130.0,32.0),(131.0,30.0),(132.0,31.0),(133.0,29.0),(134.0,29.0),(135.0,27.0),(136.0,26.0),(137.0,26.0),(138.0,25.0),(139.0,24.0),(140.0,24.0),(141.0,24.0),(142.0,23.0),(143.0,22.0),(144.0,22.0),(145.0,22.0),(146.0,22.0),(147.0,22.0),(148.0,22.0),(149.0,21.0),(150.0,20.0),(151.0,20.0),(152.0,20.0),(153.0,20.0),(154.0,20.0),(155.0,20.0),(156.0,20.0),(157.0,20.0),(158.0,20.0),(159.0,20.0),(160.0,20.0),(161.0,20.0),(162.0,19.0),(163.0,19.0),(164.0,19.0),(165.0,19.0),(166.0,19.0),(167.0,19.0),(168.0,18.0),(169.0,18.0),(170.0,17.0),(171.0,17.0),(172.0,17.0),(173.0,17.0),(174.0,17.0),(175.0,17.0),(176.0,17.0),(177.0,17.0),(178.0,17.0),(179.0,17.0),(180.0,17.0),(181.0,17.0),(182.0,17.0),(183.0,17.0),(184.0,17.0),(185.0,17.0),(186.0,17.0),(187.0,17.0),(188.0,17.0),(189.0,17.0),(190.0,17.0),(191.0,16.0),(192.0,16.0),(193.0,16.0),(194.0,16.0),(195.0,15.0),(196.0,15.0),(197.0,15.0),(198.0,15.0),(199.0,15.0),(200.0,15.0)]

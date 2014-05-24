module Statistics where

import Data.List
import System.Directory
import System.Process
import System.ShQQ

import CreateInfo
import Functions
import DataTypes
import ParseInput
import CalculateData


-- to be used    calculateLifeTime 2  30 120            with 1 = "S0" and 2 = "S1"
calculateLifeTime :: Inputs -> Int -> Int -> Int -> IO ()
calculateLifeTime input root limitFrom limitTo = do
    (tupla,deltaTfs)  <- averageLifetime input root
    let folder                  = getfolder input
        diff                    = limitTo - limitFrom
        adjTupla                = take diff $ drop limitFrom tupla
        (result, (delay,decay)) = barbattiFitting adjTupla
--    putStrLn "\nWatch out this function calculates between STEP a and b, unless you have 1 fs = 1 step, do not use the graphic label to calculate this A.L.T.\n\n"
        stringToWrite = "\n\nThe average lifetime in state " ++ (show root) ++" according to STEP interval [" ++ (show limitFrom) ++ "-" ++ (show limitTo) ++ "] is: " ++ printZ (result * deltaTfs) ++ " fs\nDecay constant: " ++ (show decay) ++ "\nDelay constant: " ++ (show delay) ++ "\n"
        fileToWriteN  = "LifeTimeInRootS" ++ (show $ pred root)  
    putStrLn stringToWrite
    writeFile fileToWriteN stringToWrite
    putStrLn $ "\nThis information has been written into file: " ++ fileToWriteN ++ "\n"


-- input <- getInputInfos "input"
-- writeFile "dataToFit" $ unlines $ map (\x -> (show $ fst x) ++ " " ++ (show $ snd x)) fitThis
graphicLifeTime2 input root = do
    atd              <- readerData
    let plottable    = getListToPlot input
        folder       = getfolder input
        fileN        = folder ++ "-Stats"
        rootString   = "S" ++ (show $ pred root)
        rightInd     = findInd Root plottable
        states       = map (map (\x -> x!!rightInd)) atd
        counter x    = if x == rootString then 1 else 0 
        number       = transpose $ map (map counter) states
        trajCounter  = length $ number !! 0 -- need to average like this because some trajectories can be shorter
        averages     = map (\x -> fromIntegral (sum x) / (fromIntegral trajCounter)) number
        tupleForFit  = zip (map (\x -> fromIntegral x :: Double) [1..]) averages
        transf (x,y) = show x ++ " " ++ show y
        tupleInFile  = unlines $ map transf tupleForFit 
        fitThis      = dropWhile (\x -> snd x == 1.0) tupleForFit -- we need to exclude values = 1
        fitData      = barbattiFitting fitThis
        t1  = printZ $ fst $ snd $ fitData 
        t2  = printZ $ snd $ snd $ fitData
        tau = printZ $ fst fitData
        resultsString = "\n\nt1 = " ++ t1 ++ "\nt2 = " ++ t2 ++ "\ntotal lifetime = " ++ tau ++ "\n"
        xrange       = "set xtics 20\nset yrange [0:1]"
        fnLabe       = "AverageOnState" ++ rootString
        fx           = "exp ((" ++ t1 ++ " - x)/" ++ t2 ++ ")"
        graphicpng   = "set title \"Average Time Into" ++ rootString ++ "\"\nset xlabel \"STEPS\"\nset output 'AvgTimeInRoot" ++ rootString ++ ".png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\n" ++ xrange ++ "\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines t 'Fraction of trajectories on " ++ rootString ++ "', " ++ fx
        graphicDumb  = "set title \"Average Time Into" ++ rootString ++ "\"\nset xlabel \"STEPS\"\nset key off\nset terminal dumb\nset yrange [0:1]\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines"
    writeFile (fnLabe ++ "gnuplotScript") graphicpng
    writeFile (fnLabe ++ "gnuplotScriptD") graphicDumb
    writeFile (fnLabe ++ "gnuplotValues") tupleInFile
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScriptD")
    system $ "rm " ++ (fnLabe ++ "gnuplotScriptD")
    appendFile fileN resultsString
    putStrLn resultsString
    putStrLn $ "\nFile AvgTimeInRoot" ++ (show root) ++ ".png written !!\n"        

-- to be used with 1 = "S0" and 2 = "S1"
graphicLifeTime :: Inputs -> Int -> IO ()
graphicLifeTime input root = do
    (tupla,deltaTfs) <- averageLifetime input root
    let folder = getfolder input
        transf (x,y) = show x ++ " " ++ show y
--        xrange = "set xrange [0:200]\nset xtics 10\nset yrange [0:1]"
        xrange = "set xtics 10\nset yrange [0:1]"
       -- fnLabe = folder ++ "/AverageOnState" ++ (show root)
        fnLabe = "AverageOnState" ++ (show root)
        graphicpng       = "set title \"Average Time Into" ++ (show root) ++ "\"\nset xlabel \"STEPS\"\nset key off\nset output 'AvgTimeInRoot" ++ (show root) ++ ".png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\n" ++ xrange ++ "\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines"
        graphicDumb       = "set title \"Average Time Into" ++ (show root) ++ "\"\nset xlabel \"STEPS\"\nset key off\nset terminal dumb\nset yrange [0:1]\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines"
    writeFile (fnLabe ++ "gnuplotScript") graphicpng
    writeFile (fnLabe ++ "gnuplotScriptD") graphicDumb
    writeFile (fnLabe ++ "gnuplotValues") $ unlines $ map transf tupla
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScriptD")
    system $ "rm " ++ (fnLabe ++ "gnuplotScriptD")
--    system $ "rm " ++ (fnLabe ++ "GnupValues") ++ " " ++ (fnLabe ++ "gnuplotScript")
    putStrLn $ "\nFile AvgTimeInRoot" ++ (show root) ++ ".png written !!\n"


-- this is the most accurate one, that calculates the average population to be used with root INDEX, 1 or 2
--averageLifetime :: Int -> IO [(Double, Double)]
averageLifetime input state = do
    let folder = getfolder input
    outs            <- readShell $ "ls INFO/*.info"
    let outputs     = lines outs
    stinGZ          <- mapM rdInfoFile outputs
    let deltaTfs    = (getDT $ head stinGZ) * convAUtoFS  -- it is not always 1 step = 1 fs. I need to convert.
        energies    = map getEnergies stinGZ
        rightstate  = state -1
        rightArray  = map (\x -> x!!rightstate) energies
        rightAverag = avgListaListe rightArray
        tupla       = zip (map fromIntegral2 [0..]) rightAverag
--        tupla       = zip (iterate (+deltaTfs) 0) rightAverag
    return (tupla,deltaTfs)

-- equation 9 and 10 http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
exponentialFitting :: [(Double, Double)] -> [Double]
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
barbattiFitting :: [(Double, Double)] -> (Double, (Double, Double))
barbattiFitting tuplas = let
    [a,b] = exponentialFitting tuplas
    t1    = -(( log a ) / b )
    t2    = (-1)/b
    tau   = t1 + t2
    in (tau,(t1,t2))

-- this HUGE function is simpler than it seems, it is using gnuplot variational method to plot the average lifetime (in barbatti way) on DATA files
graphicLifeTime3 input root = do
    atd                  <- readerData
    let plottable        = getListToPlot input
        folder           = getfolder input
        fileN            = folder ++ "-Stats"
        rootString       = "S" ++ (show $ pred root)
        rightInd         = findInd Root plottable
        states           = map (map (\x -> x!!rightInd)) atd 
        counter x        = if x == rootString then 1 else 0 
        number           = transpose $ map (map counter) states
        trajCounter      = length $ number !! 0 -- need to average like this because some trajectories can be shorter
        averages         = map (\x -> fromIntegral (sum x) / (fromIntegral trajCounter)) number
        tupleForFit      = zip (map (\x -> fromIntegral x :: Double) [1..]) averages
        transf (x,y)     = show x ++ " " ++ show y
        tupleInFile      = unlines $ map transf tupleForFit 
        fitThis          = dropWhile (\x -> snd x == 1.0) tupleForFit -- we need to exclude values = 1
        toFitInFile      = unlines $ map transf fitThis
        xrange           = "set xtics 20\nset yrange [0:1]"
        fnLabe           = folder ++ "AverageLifeTimeOn" ++ rootString
        dataToPlotName   = fnLabe ++ "ToFit"
        scriptToPlotName = fnLabe ++ "ScriptToFit"
        t1Guess          = fst $ head fitThis
        plotFileCont     = (intercalate "\n" [gnuplotFunctionBarb,("t1 = " ++ show t1Guess),(fitline dataToPlotName)]) ++ "\n"
    writeFile dataToPlotName toFitInFile
    writeFile scriptToPlotName plotFileCont
    system "rm fit.log"
    system $ "gnuplot < " ++ scriptToPlotName ++ " > /dev/null"
    t1S <- readShell $ "grep -A4 'Final set of parameters' fit.log | grep 't1' | awk '{print $3}'"
    t2S <- readShell $ "grep -A4 'Final set of parameters' fit.log | grep 't2' | awk '{print $3}'"
    let [t1,t2]              = map read2 [t1S,t2S]
        tauS                 = t1 + t2
        resultsString        = "Average LifeTime on " ++ rootString ++ ": " ++ printZ tauS     ++ "\nLatency Time   t1 = " ++ printZ t1 ++ "\nDecay Constant t2 = " ++ printZ t2 ++ "\n"
    putStrLn $ "\n" ++ resultsString ++ "\nA fit.log file has been created with more information on this fit.\n\n"
    appendFile fileN resultsString
    let  xrange       = "set xtics 20\nset yrange [0:1]"
         fnLabe       = "AverageOnState" ++ rootString
         fx           = gnuplotFunctionBarb 
         graphicpng   = "set title \"Average Time Into" ++ rootString ++ "\"\nset xlabel \"STEPS\"\nset output 'AvgTimeInRoot" ++ rootString ++ ".png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\n" ++ xrange ++ "\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines t 'Fraction of trajectories on " ++ rootString ++ "', " ++ fx
         graphicDumb  = "set title \"Average Time Into" ++ rootString ++ "\"\nset xlabel \"STEPS\"\nset key off\nset terminal dumb\nset yrange [0:1]\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines"
    writeFile (fnLabe ++ "gnuplotScript") graphicpng
    writeFile (fnLabe ++ "gnuplotScriptD") graphicDumb
    writeFile (fnLabe ++ "gnuplotValues") tupleInFile
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScriptD")
    system $ "rm " ++ (fnLabe ++ "gnuplotScriptD")
    putStrLn $ "\nFile AvgTimeInRoot" ++ (show root) ++ ".png written !!\n"

fitline filename = "fit f(x) \"" ++ filename ++ "\" u 1:2 via t1,t2"

gnuplotFunctionBarb = "f(x) = exp(-(x-t1)/t2)"

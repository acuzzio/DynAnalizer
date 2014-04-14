module Statistics where

import Data.List
import System.Directory
import System.Process
import System.ShQQ

import CreateInfo
import Functions
import DataTypes


-- to be used    calculateLifeTime 2  30 120            with 1 = "S0" and 2 = "S1"
calculateLifeTime :: Inputs -> Int -> Int -> Int -> IO ()
calculateLifeTime input root limitFrom limitTo = do
    (tupla,deltaTfs)  <- averageLifetime input root
    let folder        = getfolder input
        diff          = limitTo - limitFrom
        adjTupla      = take diff $ drop limitFrom tupla
        result        = barbattiFitting adjTupla
--    putStrLn "\nWatch out this function calculates between STEP a and b, unless you have 1 fs = 1 step, do not use the graphic label to calculate this A.L.T.\n\n"
        stringToWrite = "\n\nThe average lifetime in state " ++ (show root) ++" according to STEP interval [" ++ (show limitFrom) ++ "-" ++ (show limitTo) ++ "] is: " ++ printZ (result * deltaTfs) ++ " fs\n"
        fileToWriteN  = folder ++ "/LifeTimeIn" ++ (show root)  
    putStrLn stringToWrite
    writeFile fileToWriteN stringToWrite
    putStrLn $ "\nThis information has been written into file: " ++ fileToWriteN ++ "\n"

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
        header       = "set title \"Average Time Into" ++ (show root) ++ "\"\nset xlabel \"STEPS\"\nset key off\nset output 'AvgTimeInRoot" ++ (show root) ++ ".png'\nset terminal pngcairo size 1224,830 enhanced font \", 12\"\n" ++ xrange ++ "\nplot \"" ++ (fnLabe ++ "gnuplotValues") ++ "\" u 1:2 w lines"
    writeFile (fnLabe ++ "gnuplotScript") header
    writeFile (fnLabe ++ "gnuplotValues") $ unlines $ map transf tupla
    system $ "gnuplot < " ++ (fnLabe ++ "gnuplotScript")
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
barbattiFitting :: [(Double, Double)] -> Double
barbattiFitting tuplas = let
    [a,b] = exponentialFitting tuplas
    t1    = -(( log a ) / b )
    t2    = (-1)/b
    tau   = t1 + t2
    in tau


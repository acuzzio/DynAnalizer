module ParseInput where

import Data.Char
import Data.String.Utils
import Text.Parsec
import Text.Parsec.Combinator
--import Text.Parsec.String
import Text.Parsec.ByteString -- Felipe suggested
import Data.Functor.Identity

import DataTypes

data Inputs = Inputs {
     getfolder            :: String,        -- Here Info foldername
     getchargeTrFragment  :: [Int],         -- Here list of Atom in charge transfer fraction
     getchargeTrThresh    :: [Double],      -- Here list of Threshold for CT graphics
     getccccList          :: [Int],         -- Here the central dihedral
     getbetaList          :: [Int],         -- Here beta angle
     getblaList           :: [[(Int,Int)]], -- BLA list of single bonds, list of double bonds
     getisomType          :: IsomType,      -- This is your cutting line if you wanna shift down graphics
     getnRoot             :: Int,           -- This is the root number
     getListToPlot        :: [Plottable],   -- These are the DATA file column
     getgnuplotOptions    :: String       -- Those are gnuplot optionsin a LIST
     } deriving Show

writeInputTemplate :: FilePath -> IO()
writeInputTemplate fn = do
  let content = "chargeTrFragment = [1,2,3]                         -- Here list of Atom in charge transfer fraction\nchargeTrThresh   = [0.4,0.5,0.6]                   -- Here list of Threshold for CT graphics\nccccList   = [6,7,8,9]                             -- Here the central dihedral\nbetaList   = [19,7,8,16]                           -- Here beta angle\nblaList    = [[(2,6),(7,8),(9,10)],[(6,7),(8,9)]]  -- BLA list of single bonds, list of double bonds\nisomType   = Trans                                 -- Here Cis or Trans\nnRoot      = 2                                     -- This is the number of root in the system\ndataPlot   = [Cccc,CcccCorrected,Beta,BetaCorrected,Tau,Delta,Bla,Ct,Root,Jump] -- Data file Columns\ngnuplotOptions = set terminal pngcairo size 1224,830 enhanced font \", 12\" \\nset xrange [0:200]    -- Gnuplot options separated by \\n \n"
  putStrLn $ "Template input file: " ++ fn ++ " written."
  writeFile fn content


dataplot = [Cccc, CcccCorrected, Beta, BetaCorrected, Tau, Delta, Bla, Ct, Root, Jump]
gnuplotDefault = "set terminal pngcairo size 1224,830 enhanced font \", 12\"\nset xrange [1:10]"
defaul = Inputs "lol" [1] [0.5] [1] [1] [[(1,1)]] Cis 0 dataplot gnuplotDefault

getUpperAndIsomCond :: IsomType -> (Double, (Double -> Bool))
getUpperAndIsomCond a = case a of
     Cis    -> (90.0, (\x -> x > 90.0 || x < -90.0 ))
     Trans  -> (90.0, (\x -> x > -90.0 || x < -270.00))

getInputInfos file = do
   r <- parseFromFile parseInput file
   case r of  
     Left msg -> return defaul 
     Right x  -> return x

parseInput = do
 b <- parseLine "chargeTrFragment"
 manyTill anyChar eoL 
 let b1 = read b :: [Int]
 bb <- parseLine "chargeTrThresh"
 manyTill anyChar eoL 
 let bb1 = read bb :: [Double]
 c <- parseLine "ccccList"    
 manyTill anyChar eoL
 let c1 = read c :: [Int]
 d <- parseLine "betaList"         
 manyTill anyChar eoL
 let d1 = read d :: [Int]
 e <- parseLine "blaList"        
 manyTill anyChar eoL 
 let e1 = read e :: [[(Int,Int)]]
 f <- parseLine "isomType"
 manyTill anyChar eoL
 let f1 = read f :: IsomType
 g <- parseLine "nRoot"            
 manyTill anyChar eoL 
 let g1 = read g :: Int
 h <- parseLine "dataPlot"    
 manyTill anyChar eoL 
 let h1 = read h :: [Plottable]
 j <- parseLine "gnuplotOptions"    
 manyTill anyChar eoL 
 let j1 = replace "\\n" "\n" j
 return $ Inputs "itWillBeReplacedByfn" b1 bb1 c1 d1 e1 f1 g1 h1 j1

parseLine label = do
   many spaces2
   optional $ try $ string label
   many spaces2
   optional $ string "="
   many spaces2
   a <- manyTill anyChar eoI
   return a

eoI = try eoL
      <|> try (string "--")

eoL = try (string "\r")
      <|> try (string "\n")

spaces2 :: Parser String
spaces2 =       try (string "\r")
                <|> try (string "\n")
                <|> string "\t"
                <|> string " "


module ParseInput where

import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Functor.Identity

import DataTypes

defaul = Inputs "lol" [1] [1] [1] [[(1,1)]] Cis 3

getUpperAndIsomCond :: IsomType -> (Double, (Double -> Bool))
getUpperAndIsomCond a = case a of
     Cis    -> (90.0, (\x -> x < -90.0))
     Trans  -> (90.0, (\x -> x < -90.0))

getInputInfos file = do
   r <- parseFromFile parseInput file
   case r of  
     Left msg -> return defaul 
     Right x  -> return x

parseInput = do
 a <- parseLine "folder"
 manyTill anyChar eoL 
 b <- parseLine "chargeTrFragment"
 manyTill anyChar eoL 
 let b1 = read b :: [Int]
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
 return $ Inputs a b1 c1 d1 e1 f1 g1

parseLine label = do
   many spaces2
   optional $ try $ string label
   many spaces2
   optional $ string "="
   many spaces2
   a <- manyTill anyChar eoI
   return a

eoI = try spaces2
      <|> try (string "--")

eoL = try (string "\r")
      <|> try (string "\n")

spaces2 :: Parser String
spaces2 =       try (string "\r")
                <|> try (string "\n")
                <|> string "\t"
                <|> string " "

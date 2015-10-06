{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C

data Option = Charge [Int] 
            | ThresholdCharge [Double] 
            | ChargeAnalisys ([Int],[Double]) deriving Show

main = do
 a <- B.readFile "input2"
 case parseOnly parseChargeInp a of
    Right x -> return x
    Left _  -> error "There was an error" 

parseMany :: Parser [Option]
parseMany = C.many' parseEither 

parseEither :: Parser Option
parseEither = do
     parseChargeLine
     <|> parseChargeThre
     <|> parseChargeInp

parseChargeInp :: Parser Option
parseChargeInp = do
    C.skipSpace
    string "charge"
    C.skipSpace
    a <- decimal `sepBy` C.skipSpace
    C.skipSpace
    char '['
    C.skipSpace
    c <- double `sepBy` C.skipSpace  <* anyLine'
    return $ ChargeAnalisys (a, c)

parseChargeLine :: Parser Option
parseChargeLine = do
      C.skipSpace
      a <- string "chargeF" *> C.many' spaceDecimal <* anyLine'
      return $ Charge a

parseChargeThre :: Parser Option
parseChargeThre = do 
      C.skipSpace
      a <- string "chargeT" *> C.many' spaceDouble  <* anyLine'
      return $ ThresholdCharge a

anyLine :: Parser B.ByteString
anyLine = takeTill  (== '\n') -- whatever chars we find till we hit a newline

anyLine' :: Parser ()
anyLine' = anyLine *> endOfLine  -- whatever chars we find till we hit a newline


spaceDecimal :: Parser Int
spaceDecimal = takeWhile1 isSpace *> decimal

spaceDouble :: Parser Double
spaceDouble = takeWhile1 isSpace *> double

spaceAscii :: Parser B.ByteString
spaceAscii =  takeWhile1 isSpace *> takeWhile1 isAlpha_ascii


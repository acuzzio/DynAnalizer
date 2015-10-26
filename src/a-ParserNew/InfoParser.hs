{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
import Control.Monad

import DataTypes
import IntCoor
import ParserFunctions

fn = "HopS.info"

main = do
  a <- B.readFile fn
  case parseOnly parseInfo a of
       Left msg -> error "eeeeh InFo !@#$@#$"
       Right x  -> return x

parseInfo = do
  (nRoot,dt,aT) <- parseHeader
  let nAtom = length aT
  all <- many' (parseStep nRoot nAtom)
  return (nRoot,dt,aT,all)
  
parseHeader = do
  nRoot <- decimal <* anyLine'
  dt    <- double <* anyLine'
  aTbs  <- takeWhile1 isAlpha_ascii <* anyLine'
  let aT = B.unpack aTbs
  return (nRoot,dt,aT)

parseStep nRoot nAtom = do
  count nRoot parseWF  -- still does not take anything
  geom <- parseVector nAtom
  chargeDipo  <- count nRoot parseChargeDipo
  let charge = map fst chargeDipo
      dipo   = map snd chargeDipo
  enepop      <- parseEnePop
  grad        <- parseVector nAtom
  velo        <- parseVector nAtom
  [kin,tot]   <- parseKinTot 
  return (geom,charge,dipo,enepop,grad,velo,kin,tot)

parseKinTot = do
  kin <- double
  space
  tot <- double
  return [kin,tot]

parseEnePop = double `sepBy` " " <* anyLine'

parseChargeDipo = do
  a <- parseCharge
  b <- parseDipo
  return (a,b)

parseCharge = double `sepBy` " " <* anyLine'

parseDipo = double `sepBy` " " <* anyLine'

parseWF = do -- yeah yeah yeah, I'll do it later
  anyLine'
            
parseVector nAtom = do
  count nAtom parseTripletVec

parseTripletVec = do
  x <- double
  skipSpace
  y <- double
  skipSpace
  z <-double
  anyLine'
  return $ Vec [x,y,z]
   
 

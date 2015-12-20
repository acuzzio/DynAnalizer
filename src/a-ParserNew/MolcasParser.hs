{-# LANGUAGE OverloadedStrings #-}

module MolcasParser where

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
import Control.Monad

import ParserFunctions

--fn = "geom067.out"
fn = "HopS.out"
aaaa=":set -XOverloadedStrings"

createINFOnew fn = do
 a <- B.readFile fn
 let  infoName = (reverse (dropWhile (/= '.') $ reverse fn )) ++ "info"
 case parseOnly parseFile a of
      Left msg -> error "eeeeh"
      Right x  -> do
                  let a = B.unlines x
                  B.writeFile infoName a
--                  return a

-- this will be the main structure of the MD file !
--parseFile :: Parser [B.ByteString] 
parseFile = do
  nR     <- parseNRoot
  let nRoot = read [(B.head nR)] :: Int
  dt     <- parseDT
  aType  <- parseAtomTypes
  let nAtom = B.length aType
  all    <- many' $ parseMDStep nAtom nRoot
  return $ [nR,dt,aType] ++ all

-- This function for debug purposes
parseMDStepDBG :: Int -> Int -> Parser B.ByteString
parseMDStepDBG nAtom nRoot = do
--  a     <- B.intercalate "\n" <$> count nRoot parseWF
--  a     <- parseGeom nAtom
--  b     <- B.intercalate "\n" <$> count nRoot parseSingleCharge
--  c     <- parseEnePop
--  d     <- parseGradient nAtom
  e     <- parseInVelo nAtom
  return $ e

-- this structure is repeated nStep times
parseMDStep :: Int -> Int -> Parser B.ByteString
parseMDStep nAtom nRoot = do
  geom     <- parseGeom nAtom
  wf       <- B.intercalate "\n" <$> count nRoot parseWF
  charge   <- B.intercalate "\n" <$> count nRoot parseChargeDipole
  enepop   <- parseEnePop
  grad     <- parseGradient nAtom
  velo     <- parseInVelo nAtom
  kintot   <- parseKinTot
  return $ B.intercalate "\n" [wf,geom,charge,enepop,grad,velo,kintot]

parseWF :: Parser B.ByteString
parseWF = do
  let start = "conf/sym"
      stop1  = "printout"
      stop2  = "Natural"
  skipTill start
  count 1 anyLine'
  whilePatt2 stop1 stop2 parseSingleLineWF

parseSingleLineWF :: Parser B.ByteString
parseSingleLineWF = do
  a <- anyLine <* endOfLine
  return $ B.append (trimDoubleSpaces a) (" ")

parseKinTot :: Parser B.ByteString
parseKinTot = do
  let condition x =  any (==x) [' ','\n'] 
  skipTillCase "kinetic energy" 
  option "" (skipSpace *> string "(Hartree) is:")
  x <- skipSpace *> takeTill condition
  skipTillCase "total energy"
  option "" (skipSpace *> string "(Hartree) is:")
  y <- skipSpace *> takeTill condition
  let replaceD = map (\c -> if c =='D' then 'E'; else c)
  return $ B.pack $ replaceD $ B.unpack $ B.unwords [x,y]
  --return $ replaceD $ B.unwords [x,y]
  
parseInVelo :: Int -> Parser B.ByteString
parseInVelo atomN = do 
--  skipTillSafe "Velocities" "SEWARD"  
  skipTill "Velocities"  
  count 4 anyLine'
  a <- count atomN $ veloLine
  return $ treatTriplets a

veloLine :: Parser B.ByteString
veloLine = do 
  skipSpace *> decimal
  spaceAscii
  let condition x = x == ' ' || x == '\n'
  x <- skipSpace *> takeTill (== ' ')
  y <- skipSpace *> takeTill (== ' ')
  z <- skipSpace *> takeTill (condition)
  anyLine'
  return $ B.unwords [x,y,z]

pp = B.putStrLn . B.unlines

ppp x = B.putStrLn $ B.unlines [x]

-- seek for "ciroot" keyword in molcas input section of the output.
parseNRoot :: Parser B.ByteString
parseNRoot = do
  skipTillCase "ciro"
  count 1 anyLine'
  a <- anyLine
  return $ trimDoubleSpaces a

-- seek for the string "dt" (in any case) in the input section of the output
parseDT :: Parser B.ByteString
parseDT = do
  skipTillCase "dt" 
  count 1 anyLine'
  a <- anyLine
  return $ trimDoubleSpaces a

parseAtomTypes :: Parser B.ByteString
parseAtomTypes = do
  skipTill "Molecular structure info:"
  skipTill "Center  Label"
  count 1 anyLine'
  a <- whilePatt "*************" atomTypeLineParser
  return a

atomTypeLineParser :: Parser B.ByteString
atomTypeLineParser = skipSpace *> decimal *> skipSpace *> takeWhile1 isAlpha_ascii <* anyLine'

-- enters in Alaska and parse out the gradient
parseGradient :: Int -> Parser B.ByteString
parseGradient atomN = do
  let start = "Molecular gradients"
  skipTill start
  count 8 anyLine'
  a <- count atomN $ spaceAscii *> decimal *> skipSpace *> anyLine
  return $ treatTriplets a 

-- at step 1 the code prints "Cannot do deltas" and the other steps are "\n --- Stop module". This is why we stop the parser to both letter C and S, filtering out the "---" string.
parseEnePop :: Parser B.ByteString
parseEnePop     = do
  let start     = "OOLgnuplt:"
      transform = B.unwords . filter (/= "---") . B.words
  skipTill start
  skipSpace
  a <- takeTill (\x -> x == 'S' || x == 'C')
  return $ transform a

parseGeom :: Int -> Parser B.ByteString
parseGeom atomN = do
  manyTill anyChar (string "Cartesian coordinates in Angstrom:") 
  count 4 anyLine' 
  a <- count atomN $ skipSpace *> decimal *> spaceAscii *> decimal *> skipSpace *> anyLine
  return $ treatTriplets a

-- this parse the mulliken charge + the dipole
parseChargeDipole :: Parser B.ByteString
parseChargeDipole = do
  a <- parseSingleCharge 
  b <- parseDipole
  return $ B.intercalate "\n" [a,b]

parseDipole :: Parser B.ByteString
parseDipole = do
  skipTill "Dipole Moment (Debye):"
  count 2 anyLine'
  skipSpace *> string "X=" 
  x <- skipSpace *> takeTill (== ' ')
  skipSpace *> string "Y=" 
  y <- skipSpace *> takeTill (== ' ')
  skipSpace *> string "Z=" 
  z <- skipSpace *> takeTill (== ' ')
  return $ B.unwords [x,y,z]

parseSingleCharge :: Parser B.ByteString
parseSingleCharge = do
  let start     = "Mulliken charges per centre and basis function type"
      stop      = "Total electronic charge="
  skipTill start
  withSpaces <- whilePatt stop (lineChargePattern "N-E")
  return $ trimDoubleSpaces withSpaces

lineChargePattern :: B.ByteString -> Parser B.ByteString
lineChargePattern pat = findPattern <|> (anyLine' *> pure "")
  where findPattern = spaces *> string pat *> anyLine <* endOfLine


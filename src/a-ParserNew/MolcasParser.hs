{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
import Control.Monad

main = do
 a <- B.readFile "geom067S.out"
 case parseOnly parseFile a of
      Left msg -> error "eeeeh"
      Right x  -> return x

-- this will be the main structure of the MD file
--parseFile :: Parser [B.ByteString] 
parseFile = do
  nR    <- parseNRoot
  let nRoot = read [(B.head nR)] :: Int
  dt    <- parseDT
  aType <- parseAtomTypes
  let nAtom = B.length aType
  a     <- parseGeom nAtom
  b     <- B.intercalate "\n" <$> count nRoot parseSingleCharge
  c     <- parseEnePop
  cc    <- parseGradient nAtom
  --return (nR,dt,aType,a,b,c,cc,d)
  return [nR,dt,aType,a,b,c,cc]

pp = B.putStrLn . B.unlines

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
  return $ B.unlines $ map trimDoubleSpaces a

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
  return $ B.unlines $ map trimDoubleSpaces a

parseSingleCharge :: Parser B.ByteString
parseSingleCharge = do
  let start     = "Mulliken charges per centre and basis function type"
      stop      = "Total electronic charge="
  skipTill start
  withSpaces <- whilePatt stop (lineChargePattern "N-E")
  return $ trimDoubleSpaces withSpaces

-- repeat the parser N times and concatenate results
repeatParserNTimes :: Int -> Parser B.ByteString -> Parser B.ByteString
repeatParserNTimes n p = loop n B.empty
  where loop n acc = do
           xs <- p
           if n == 0 then pure acc
                     else let rs = B.append acc xs
                          in loop (n-1) rs

-- repeat the parser until the "stop" string is found
whilePatt :: B.ByteString -> Parser B.ByteString -> Parser B.ByteString 
whilePatt stop p = loop B.empty 
  where loop acc = do
           xs <- (spaces *> string stop ) <|>  p
           if xs == stop then pure acc
                         else let rs = B.append acc xs
                              in loop rs

lineChargePattern :: B.ByteString -> Parser B.ByteString
lineChargePattern pat = findPattern <|> (anyLine' *> pure "")
  where findPattern = spaces *> string pat *> anyLine <* endOfLine

---- Get the atom numbers from Gateway
--countAtoms :: Parser Int 
--countAtoms = do
--     manyTill anyChar (string "Center  Label")  *> anyLine'
--     xs <- B.unpack <$> takeTill (== '*')
--     return $ length $ filter (isAlpha_ascii . head ) $ words xs

anyLine :: Parser B.ByteString
anyLine = takeTill  (== '\n')
 
anyLine' :: Parser ()
anyLine' = anyLine *> endOfLine 

-- "     12"
spaceDecimal :: Parser Int
spaceDecimal = takeWhile1 isSpace *> decimal

-- "     12.12"
spaceDouble :: Parser Double
spaceDouble = takeWhile1 isSpace *> double

-- "     asdsa"
spaceAscii :: Parser B.ByteString
spaceAscii =  takeWhile1 isSpace *> takeWhile1 isAlpha_ascii

-- throw away everything until the string pattern
skipTill :: B.ByteString -> Parser ()
skipTill pattern = skipWhile (/= head (B.unpack pattern)) *> ( (string pattern *> pure () )  <|> (anyChar *> skipTill pattern))

-- throw away everything until the string pattern CASE UNSEnSiTiVE
skipTillCase :: B.ByteString -> Parser ()
skipTillCase pattern = do
  let firstLetter = toUpper . head $ B.unpack pattern
      condition fL x = all (/=x) [fL, toLower fL] 
      --x /= fL && x /= (toLower fL)
  skipWhile (\x -> condition firstLetter x) *> ( (stringCI pattern *> pure () )  <|> (anyChar *> skipTillCase pattern))

-- transform " 34 12 123    1234  1234  " into "34 12 123 1234 1234"
trimDoubleSpaces :: B.ByteString -> B.ByteString
trimDoubleSpaces = B.unwords . B.words

spaces :: Parser ()
spaces = skipWhile isSpace

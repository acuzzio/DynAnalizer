{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (pure, (<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 

main = do
 a <- B.readFile "geom067S.out"
 case parseOnly parseFile a of
      Left msg -> error "eeeeh"
      Right x  -> return x

parseFile = do
  nAtom <- countAtoms
  a <- parseGeom nAtom
  b <- parseCharge
  return (a,b)

parseGeom :: Int -> Parser B.ByteString
parseGeom atomN = do
  manyTill anyChar (string "Cartesian coordinates in Angstrom:") 
  count 4 anyLine' 
  parseSingleGeometry atomN

parseGeom2 :: Int -> Parser B.ByteString
parseGeom2 atomN = do
  manyTill anyChar (string "Cartesian coordinates in Angstrom:") 
  count 4 anyLine' 
  parseSingleGeometry atomN

parseCharge :: Parser B.ByteString
parseCharge :: Parser B.ByteString
parseCharge = do
   let start     = "Mulliken charges per centre and basis function type"
       stop      = "Total electronic charge="
   skipTill start
   whilePatt stop (linePattern "N-E")

whilePatt :: B.ByteString -> Parser B.ByteString -> Parser B.ByteString 
whilePatt stop p = loop B.empty 
   where loop acc = do
           xs <- (spaces *> string stop ) <|>  p
           if xs == stop then pure acc
                         else let rs = B.append acc xs
                              in loop rs 
 
linePattern :: B.ByteString -> Parser B.ByteString
linePattern pat = findPatern <|> (anyLine' *> pure "")
 where       
       findPatern = spaces *> string pat *> anyLine <* endOfLine
 

parseSingleGeometry :: Int -> Parser B.ByteString
parseSingleGeometry atomN = do
    a <- count atomN $ skipSpace *> decimal *> spaceAscii *> decimal *> skipSpace *> anyLine
    return $ B.unlines a

countAtoms :: Parser Int 
countAtoms = do
     manyTill anyChar (string "Center  Label")  *> anyLine'
     xs <- B.unpack <$> takeTill (== '*')
     return $ length $ filter (isAlpha_ascii . head ) $ words xs
            

anyLine :: Parser B.ByteString
anyLine = takeTill  (== '\n')
 
anyLine' :: Parser ()
anyLine' = skipWhile (/= '\n') *> endOfLine 

spaceDecimal :: Parser Int
spaceDecimal = spaces *> decimal

spaceDouble :: Parser Double
spaceDouble = spaces *> double

spaceAscii :: Parser B.ByteString
spaceAscii =  spaces *> takeWhile1 isAlpha_ascii

spaces :: Parser ()
spaces = skipWhile isSpace

-- | Skip Chars until the pattern is found 
skipTill' :: Char -> B.ByteString -> Parser ()
skipTill' letter pat = skipWhile (/= letter) *> ( (string pat *> pure () )  <|> (anyChar *> skipTill' letter pat))

skipTill :: B.ByteString -> Parser ()
skipTill pattern = skipWhile (/= head (B.unpack pattern)) *> ( (string pattern *> pure () )  <|> (anyChar *> skipTill pattern))

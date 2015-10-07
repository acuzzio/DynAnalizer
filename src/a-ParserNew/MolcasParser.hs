{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C

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

parseCharge :: Parser B.ByteString
parseCharge = do
   let start = "Mulliken charges per centre and basis function type"
       stop  = "Total electronic charge="
   a <- manyTill anyChar (string start) *>  manyTill anyChar (string stop) 
   let match = "N-E" 
   return $ transformCharge match a 
   
transformCharge :: String -> String -> B.ByteString
transformCharge str x =  B.unwords . concat . map tail . filter (\x -> (head x) == (B.pack str)) . filter (/= []) . map B.words . B.lines $ B.pack x

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
anyLine' = anyLine *> endOfLine 

spaceDecimal :: Parser Int
spaceDecimal = takeWhile1 isSpace *> decimal

spaceDouble :: Parser Double
spaceDouble = takeWhile1 isSpace *> double

spaceAscii :: Parser B.ByteString
spaceAscii =  takeWhile1 isSpace *> takeWhile1 isAlpha_ascii


{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C

main = do
 a     <- B.readFile "geom067S.out"
 case parseOnly (manyTill parseGeom a) of
      Left msg -> error "eeeeh"
      Right x  -> print x

parseGeom :: Parser B.ByteString
parseGeom = do
  manyTill anyChar (string "Cartesian coordinates in Angstrom:") *> count 4 anyLine' 
  a <- takeTill (== '=')
  return a

anyLine :: Parser B.ByteString
anyLine = takeTill  (== '\n')
 
anyLine' :: Parser ()
anyLine' = anyLine *> endOfLine 



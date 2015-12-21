{-# LANGUAGE OverloadedStrings #-}

module ParserFunctions where

import Control.Applicative (pure,(<|>),(*>),(<*),(<$>),(<*>))
import qualified Data.ByteString.Char8  as B
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (toUpper,toLower)
--import Control.Monad

-- repeat the parser until the "stop" string is found
whilePatt :: B.ByteString -> Parser B.ByteString -> Parser B.ByteString 
whilePatt stop p = loop B.empty 
  where loop acc = do
           xs <- (spaces *> string stop) <|>  p
           if xs == stop then pure acc
                         else let rs = B.append acc xs
                              in loop rs

-- repeat the parser until the "stop1" OR "stop2" string is found
whilePatt2 :: B.ByteString -> B.ByteString -> Parser B.ByteString -> Parser B.ByteString 
whilePatt2 stop1 stop2 p = loop B.empty 
  where loop acc = do
           xs <- (spaces *> (string stop1 <|> string stop2)) <|> p
           if xs == stop1 || xs == stop2 then pure acc
                         else let rs = B.append acc xs
                              in loop rs

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

-- transform " 34 12 123    1234  1234  " into "34 12 123 1234 1234"
trimDoubleSpaces :: B.ByteString -> B.ByteString
trimDoubleSpaces = B.unwords . B.words

treatTriplets = B.init . B.unlines . map trimDoubleSpaces 

-- throw away everything until the string pattern
skipTill :: B.ByteString -> Parser ()
skipTill pattern = skipWhile funL *> ( (string pattern *> pure () )  <|> (anyChar *> skipTill pattern))
 
  where funL = (/= l)
        l    = B.head pattern
 
-- throw away everything until the string pattern CASE UNSEnSiTiVE
skipTillCase :: B.ByteString -> Parser ()
skipTillCase pattern = skipWhile (\x -> condition l x) *> ( (stringCI pattern *> pure () )  <|> (anyChar *> skipTillCase pattern))
 
  where l = firstLetterU pattern
 
-- throw away everything until patternRight is found. In case patternWrong is found I will fail/return error/return a certain string with the parser
skipTillSafe :: B.ByteString -> B.ByteString -> Parser ()
skipTillSafe patternRight patternWrong =  skipWhile condition2 *> controlLetter
  
  where controlLetter = do 
              stringCI patternRight *> pure ()
          <|> stringCI patternWrong *> pure () --fail "Porco Dio" 
          <|> anyChar *> skipTillCaseSafe patternRight patternWrong

        condition2 x =  part1 x  && part2 x 
        part1 x    = lRight /= x
        part2 x    = lWrong /= x
        lRight     = firstLetter patternRight 
        lWrong     = firstLetter patternWrong
 
-- throw away everything until patternRight is found. In case patternWrong is found I will fail/return error/return a certain string with the parser. CAse UnSeNSItive.
skipTillCaseSafe :: B.ByteString -> B.ByteString -> Parser ()
skipTillCaseSafe patternRight patternWrong =  skipWhile condition2 *> controlLetter
  
  where controlLetter = do 
              stringCI patternRight *> pure ()  
          <|> stringCI patternWrong *> pure () --fail "Porco Dio" 
          <|> anyChar *> skipTillCaseSafe patternRight patternWrong

        condition2 x =  part1 x  && part2 x 
        part1      = condition lRight
        part2      = condition lWrong 
        lRight     = firstLetterU patternRight 
        lWrong     = firstLetterU patternWrong
 
firstLetter :: B.ByteString -> Char
firstLetter  = B.head

firstLetterU :: B.ByteString -> Char
firstLetterU  = toUpper .  B.head
 
condition :: Char -> Char -> Bool
condition l x = all (/=x) [l, toLower l]
 
skipWord :: Parser ()
skipWord = spaces *> skipWhile isAlpha_ascii
 
spaces :: Parser ()
spaces = skipWhile isSpace



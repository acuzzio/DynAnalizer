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

-- | Throw away everything until the string pattern
--   TODO nota ho aggiunto qua il parametro ignoreCase tanto e` un test veloce che non rallenta niente
--
skipTill :: Bool -> B.ByteString -> Parser ()
skipTill ignoreCase pattern = consume patternStart
  
  where

    patternStart = B.uncons pattern
    -- NOTE: B.uncons is a fast operation on bytestrings and it does not involve any copy.
    -- TODO in case this is not the case, we can change the function argument to `[Char8]` and use list operations. I'm saying this because probably the argument is always specified from the user of the function, and it can be sent in list format without any problem.

    consume Nothing = do
      return ()
      -- we parsed the complete `pattern` string, and so we can stop

    consume (Just (ch1, rs)) = do
      nch <- peekChar
      -- NOTE: retrieve the current character, but without consuming it

      case nch of
        Nothing
          -> return ()
             -- reached end of input

        Just ch2
          -> case (ch1 == ch2) || (ignoreCase && (B.toLower ch1 == B.toLower ch2)) of
               True
                 -> do anyChar
                       consume (B.uncons rs)
                       -- we found the next valid character of the `pattern` string,
                       -- so we can continue parsing input searching for the next valid character
               False
                 -> do anyChar
                       consume patternStart
                       -- the character of the input is not valid, so we continue parsing it,
                       -- searching again the `pattern` string from the beginning.

-- throw away everything until the string pattern
skipTill_slow :: B.ByteString -> Parser ()
skipTill_slow pattern = skipWhile funL *> ( (string pattern *> pure () )  <|> (anyChar *> skipTill pattern))

  where funL = (/= l)
        l    = B.head pattern

-- throw away everything until the string pattern CASE UNSEnSiTiVE
-- TODO questa potresti cancellarla e chiamara la versione generica con il parametro ignoreCase
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



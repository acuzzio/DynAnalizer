{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.ByteString as DAB
import Data.Word (Word8)
import Data.Word8 (isSpace)
import qualified Data.ByteString.Char8 as C

main = do
 putStrLn ":set -XOverloadedStrings"

data Inputs = Inputs [Options] deriving Show

data Options = Charge [Int] | Internal deriving Show

parseOpt :: Parser Inputs
parseOpt = do
  parseHeader
  opts <- many1 parseOptions
  endOfInput
  return $ Inputs opts

parseOptions :: Parser Options
parseOptions = do
  skipSpace
  a <- parseLabel
  skipWhite
  case a of 
    "charTrF" -> do
                 skipSpace
                 b <- many' $ string "="
                 skipSpace
                 b <- parseList
                 return $ Charge $ map (readI . C.unpack) b
    "a"       -> return Internal

parseLabel = string "a" <|> string "charTrF" 

parseHeader :: Parser()
parseHeader = do
  skipSpace >> string "--" >> skipLine

parseInteger = do
       a <- DAB.takeWhile (inClass "0-9")
       skipSpace
       return a

parseList = manyTill parseInteger eoL

readD :: String -> Double
readD = read

readI :: String -> Int
readI = read


skipLine :: Parser ()
skipLine = skipWhile (not . isEOL) >> skipWhite

skipSpace :: Parser ()
skipSpace = skipWhile (inClass " \t")

skipWhite :: Parser()
skipWhite = skipWhile isSpace

isEOL :: Word8 -> Bool
isEOL x = x == 10 || x == 13

eoL = try (string "\r")
      <|> try (string "\n")

spaces2 =       try (string "\r")
                <|> try (string "\n")
                <|> string "\t"
                <|> string " " 


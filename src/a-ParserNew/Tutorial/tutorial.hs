{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.Char8
import Data.Word
import Data.Time
import Control.Applicative
import qualified Data.ByteString as B

data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

main :: IO ()
main = do
   print $ parseOnly timeParser "2013-06-30 14:33:29"
   print $ parseOnly parseIP "131.45.68.123"
   print $ parseOnly productParser "mouse"
   print $ parseOnly logEntryParser "2013-06-29 11:16:23 124.67.34.60 keyboard"
   B.readFile "logFile" >>= print . parseOnly logParser

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data LogEntry =
  LogEntry { entryTime :: LocalTime -- 2013-06-29 11:16:23.
           , entryIP   :: IP
           , entryProduct   :: Product
             } deriving Show

type Log = [LogEntry]
-- 2013-06-29 11:16:23 124.67.34.60 keyboard
-- 2013-06-29 11:32:12 212.141.23.67 mouse
-- 2013-06-29 11:33:08 212.141.23.67 monitor
-- 2013-06-29 12:12:34 125.80.32.31 speakers
-- 2013-06-29 12:51:50 101.40.50.62 keyboard
-- 2013-06-29 13:10:45 103.29.60.13 mouse

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }


productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
     <|> (string "keyboard" >> return Keyboard)
     <|> (string "monitor"  >> return Monitor)
     <|> (string "speakers" >> return Speakers)


logEntryParser :: Parser LogEntry
logEntryParser = do
  -- First, we read the time.
  t <- timeParser
  -- Followed by a space.
  char ' '
  -- And then the IP of the client.
  ip <- parseIP
  -- Followed by another space.
  char ' '
  -- Finally, we read the type of product.
  p <- productParser
  -- And we return the result as a value of type 'LogEntry'.
  return $ LogEntry t ip p

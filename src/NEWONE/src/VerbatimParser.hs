{-# LANGUAGE DeriveDataTypeable #-}


module VerbatimParser where

import Control.Applicative ((<$>))
import Data.Data
import Data.Typeable
import Text.Parsec
import Text.Parsec.String (Parser)


-- =============> Data Types <===========
data Verbatim =   DatVerbatim Verbatim Verbatim
               | PlainDat  String
               | EMetaVar String 
               deriving (Data,Show,Typeable)

printVerbatim :: Verbatim -> String
printVerbatim (DatVerbatim sh1 sh2) = (printVerbatim sh1) ++ (printVerbatim sh2)
printVerbatim (PlainDat s)       = s
printVerbatim (EMetaVar v)       =  "EMetaVar  " ++  v

-- ============> <===================
parseExp :: Monad m => String -> m Verbatim
parseExp s = 
            case runP parseVerbatim () "" s  of
                 Left err -> fail . show $ err 
                 Right xs -> return xs 


parseVerbatim :: Parser Verbatim
parseVerbatim = (option ' ' newline) >> (pTerm `chainl1` (return DatVerbatim))

pTerm :: Parser Verbatim
pTerm = pPlainTex <|> pMetaVar

pPlainTex :: Parser Verbatim
pPlainTex = fmap PlainDat $ many1 $ noneOf ['%']

pMetaVar :: Parser Verbatim 
pMetaVar = char '%' >> EMetaVar <$> ident

small   = lower <|> char '_'
large   = upper
idchar  = small <|> large <|> digit <|> char '\''
ident = do { c <- small; cs <- many idchar; return (c:cs) }



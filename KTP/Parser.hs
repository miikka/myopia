module KTP.Parser where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Char
import qualified Data.Map            as M
import           Text.Parsec
import           Text.Parsec.String

import           KTP.AST

brackets, parens :: Parser a -> Parser a
brackets = between (char '[' >> spaces) (spaces >> char ']')
parens = between (char '(' >> spaces) (spaces >> char ')')

comma :: Parser ()
comma = spaces >> char ',' >> spaces

number :: Parser Integer
number = read <$> many1 digit

nameParser :: Parser FunName
nameParser = do
    a <- satisfy isLower
    as <- many1 alphaNum
    return $ a:as

exprParser :: Parser Expr
exprParser = fcp <|> sp <|> zp <|> ip <|> cp <|> pp <|> mp
  where
    fcp = (FC <$> nameParser) <?> "function call"
    sp = char 'S' >> return S
    zp = char 'Z' >> return Z
    ip = char 'I' >> brackets (do
        i <- read <$> many1 digit
        comma
        k <- number
        return $ I i k)
    cp = char 'C' >> parens (do
        h <- exprParser
        comma
        gs <- exprParser `sepBy1` comma
        return $ C h gs)
    pp = char 'P' >> parens (do
        g <- exprParser
        comma
        h <- exprParser
        return $ P g h)
    mp = char 'M' >> parens (M <$> exprParser)

def :: Parser (String, Expr)
def = (do
    name <- many1 alphaNum
    spaces
    void $ char '='
    spaces
    expr <- exprParser
    return (name, expr)) <?> "definition"

commentSpaces :: Parser ()
commentSpaces = skipMany (comment <|> void space)

comment :: Parser ()
comment = string "--" >> skipMany1 (noneOf "\n") <?> "comment"

linespace :: Parser ()
linespace = skipMany $ oneOf " \t"

linebreak :: Parser ()
linebreak = linespace >> optional comment >> newline >> commentSpaces

program :: Parser Program
program = do
    commentSpaces
    defs <- def `sepEndBy1` linebreak
    eof
    return $ M.fromList defs

parseFile :: FilePath -> IO Program
parseFile fp = do
    result <- parseFromFile program fp
    case result of
        Left err -> error $ show err
        Right expr -> return expr

-- vim: ts=4 sw=4

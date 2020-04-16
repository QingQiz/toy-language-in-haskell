module Parser where

import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative


newtype Parser a = Parser {
        parse :: String -> Maybe (a, String)
    }

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    -- pa :: Parser a
    fmap f pa = Parser $ \inp ->
        case parse pa inp of 
            Nothing -> Nothing
            Just (x, xs) -> Just (f x, xs)

instance Applicative Parser where
    -- pure :: a -> f a
    pure x = Parser $ \s -> Just (x, s)
    -- (<*>) :: f (a -> b) -> f a -> f b
    pab <*> pa = Parser $ \inp ->
        case parse pab inp of
            Nothing -> Nothing
            Just (fab, rst) -> parse (fab <$> pa) rst

instance Monad Parser where
    -- (>>=) :: m a -> (a -> m b) -> m b
    pa >>= apb = Parser $ \inp ->
        case parse pa inp of
            Nothing -> Nothing
            Just (a, rst) -> parse (apb a) rst

instance Alternative Parser where
    -- empty :: f a
    empty = Parser $ const Nothing
    -- (<|>) :: f a -> f a -> f a
    pa <|> pb = Parser $ \inp ->
        case parse pa inp of
            Nothing -> parse pb inp
            x -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ func where
    func "" = Nothing
    func (x:xs) | f x = Just (x, xs)
                | otherwise = Nothing

-- parse a char
char :: Char -> Parser Char
char c = satisfy (==c)

-- parse a sapce
space :: Parser Char
space = satisfy isSpace

-- parse a number
digit :: Parser Char
digit = satisfy isDigit

-- parse a letter
letter :: Parser Char
letter = satisfy isLetter

-- parse a char belongs to cs
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- parse a string
string :: String -> Parser String
string "" = return ""
string str@(x:xs) = do
    s <- char x
    ss <- string xs
    return str

-- parse sth divied by sth
sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy sep a = do
    m1 <- a
    m2 <- many (sep >> a)
    return $ m1 : m2

-- parse sth divied by sth (result can be [])
sepByE :: Parser sep -> Parser a -> Parser [a]
sepByE sep a = sepBy sep a <|> return []
    
-- parse the left combined chain
chainl :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl op p = do
    x <- many space >> p
    for_rest x where
        for_rest x = (do
            f <- many space >> op
            y <- p
            for_rest $ f x y) <|> return x

-- parse the right combined chain
chainr :: Parser (a -> a -> a) -> Parser a -> Parser a
chainr op p = do
    x <- p
    (do f <- op
        rest <- chainr op p
        return (f x rest)) <|> return x

-- parse unary operation chain
unaryOpChain:: Parser (a -> a) -> Parser a -> Parser a
unaryOpChain op p = do
    f <- many space >> many (many space >> op)
    x <- p
    return $ func f x where
        func [] x = x
        func (f1:fs) x = func fs $ f1 x


-- parse number not include 0
number_n0 :: Parser Char
number_n0 = oneOf "123456789"

-- parse unsigned int
u_integer :: Parser Int
u_integer = read <$> ((do
    d1 <- many space >> number_n0
    dx <- many digit
    return $ d1 : dx) <|> string "0")

-- parse int
integer :: Parser Int
integer = do
    many space
    op <- string "+" <|> string "-" <|> string ""
    d  <- many space >> u_integer
    case op of
        ""  -> return d
        "+" -> return d
        "-" -> return (-1 * d)


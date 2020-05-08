{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative


newtype Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

data ParseError = DefaultError String
                | ErrorWithPos String Pos
                | EofError Pos
                deriving (Eq, Ord, Show)

newtype PString = PString (String, Pos) deriving (Show)

newtype Parser a = Parser {
        -- ExceptT :: m (Either a b) -> ExceptT a m b
        -- State s a :: s -> (a, s)
        -- State PString (Either ParseError a) >> PString -> (Either ParseError a, PString)
        runParser :: ExceptT ParseError (State PString) a
    }
    deriving (Functor, Applicative, Monad, MonadError ParseError)


instance Alternative Parser where
    empty = throwError $ DefaultError ""
    p1 <|> p2 = p1 <?> p2


try :: Parser a -> Parser a
try p = Parser (lift get) >>= \s -> p `catchError` \e -> Parser $ lift (put s) >> throwError e

infixl 4 <?>
p1 <?> p2 = try p1 `catchError` \_ -> try p2

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    PString s <- Parser $ lift get
    case s of
        ("", p) -> throwError $ EofError p
        (x:xs, pos@(Pos (a, b)))
            | f x ->
                let
                    pos' = case x of
                        '\t' -> Pos (a, b + 8)
                        '\n' -> Pos (a + 1, 1)
                        _    -> Pos (a, b + 1)
                in
                    Parser $ lift (put $ PString (xs, pos')) >> return x
            | otherwise ->
                throwError $ ErrorWithPos ("Unexcepted char " ++ show x) pos

-- get current position
peek = (\(PString (s, p)) -> p) <$> (Parser $ lift get)

parse :: Parser a -> String -> Either ParseError a
parse p s = case (runState . runExceptT . runParser) p (PString (s, Pos (1, 1))) of
    (Left err, _) -> Left (DefaultError $ showErr err s)
    (Right r,  _) -> Right r


showErr :: ParseError -> String -> String
showErr e inp = case e of
    DefaultError s              -> s
    EofError       (Pos (a, b)) -> show a ++ ":" ++ show b ++ ": \x1b[91mparse error\x1b[0m: End of Input"
    ErrorWithPos s (Pos (a, b)) -> let n = length $ show a in
        show a ++ ":" ++ show b ++ ": \x1b[91mparse error\x1b[0m:\n" ++ "\n" ++
        replicate n ' ' ++ " \x1b[94m|\n" ++
        show a ++ " | \x1b[0m" ++ lines inp !! (a - 1) ++ "\n" ++
        replicate n ' ' ++ " \x1b[94m|\x1b[91m " ++ replicate (b - 1) ' ' ++ "^\x1b[0m\n"


catchPErr :: Parser a -> String -> Parser a
catchPErr pa s = pa `catchError` \err -> case err of
    ErrorWithPos e p -> throwError $ ErrorWithPos (e ++ "\n - " ++ s) p
    a                -> throwError a


satOrError :: (Char -> Bool) -> String -> Parser Char
satOrError f s = catchPErr (satisfy f) s


putErr (Left (DefaultError a)) = putStrLn a

------------------------------------------------------------------
--                 some instances of Parser                     --
------------------------------------------------------------------
char c   = satOrError (==c)       $ "Excepted char: " ++ show c
space    = satOrError isSpace     $ "Excepted a space"
digit    = satOrError isDigit     $ "Excepted a digit"
letter   = satOrError isLetter    $ "Excepted a letter"
oneOf cs = satOrError (`elem` cs) $ "Excepted one char of [" ++ cs ++ "]"

eof = do
    PString s <- Parser $ lift get
    case s of
        ("", _) -> return ""
        (x , p) -> throwError $ ErrorWithPos ("Unexcepted string " ++ show x) p


string str = string' str `catchPErr` ("Excepted string: " ++ show str) where
    string' "" = return ""
    string' str@(x:xs) = (char x >> string' xs >> return str)

uInt :: Parser Int
uInt = (fmap read $ ((:) <$> (many space >> (oneOf "123456789")) <*> many digit) <?> string "0")

sInt :: Parser Int
sInt = (string "+" <?> string "-" <?> string "") >>= toInt (many space >> uInt) where
    toInt ps c | c == "-" = negate <$> ps
               | otherwise = ps

-- parse `space space a' or `a'
spcChar inp = many space >> char inp
spcStr  inp = many space >> string inp

-- parse `space space a space space' or `a space space'
strWithSpc inp = spcStr inp <* some space

-- parse `a sep a sep a' or `a'
sepBy sep a = (:) <$> a <*> many (sep >> a)

-- parse `a sep a sep a' or `a' or `'
sepByE sep a = sepBy sep a <?> return []

-- parse `((a op b) op c)'
chainl :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl op p = (many space >> p) >>= for_rest where
    for_rest x = ((many space >> op) <*> return x <*> p >>= for_rest) <?> return x

-- parse `op (op (op a))'
unaryOpChain:: Parser (a -> a) -> Parser a -> Parser a
unaryOpChain op p = (many (many space >> op)) >>= trans p where
    trans x fs = foldr (\f z -> f <$> z) x fs


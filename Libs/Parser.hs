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


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parse where

import AST
import Control.Applicative
import Control.Monad
import Data.Char
import Text.Read

newtype Parser a = Parser {runParser :: String -> Either (Maybe String) (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s) <- p s
    pure (f a, s)

instance Applicative Parser where
  pure v = Parser $ \s -> Right (v, s)
  (<*>) = ap

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> do
    (a, s') <- p s
    let (Parser p') = f a
    p' s'

instance Alternative Parser where
  empty = Parser $ \_ -> Left Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> case p1 s of
    Left (Nothing) -> p2 s
    Left (Just e) -> case p2 s of
      Left (Nothing) -> Left $ Just e
      Left (Just e') -> Left $ Just $ e ++ "\nOR " ++ e'
      Right v -> Right v
    Right v -> Right v

parseErr :: a -> Either (Maybe a) b
parseErr s = Left $ Just s

errParser :: String -> Parser a
errParser s = Parser $ const (parseErr s)

consumeCErr :: Char -> Parser ()
consumeCErr c = Parser $ \case
  (x : xs) | x == c -> Right ((), xs)
  (x : _) -> parseErr $ "Expected '" ++ [c] ++ "', got '" ++ [x] ++ "'"
  [] -> parseErr $ "Expected '" ++ [c] ++ "', got empty string"

consumeC :: Char -> Parser ()
consumeC c = Parser $ \case
  (x : xs) | x == c -> Right ((), xs)
  _ -> Left Nothing

consumeSpaces :: Parser ()
consumeSpaces = Parser $ \s -> Right ((), dropWhile isSpace s)

parseDef :: Parser AST
parseDef = do
  consumeC ':'
  consumeSpaces
  name <- parseIdentS
  body <- parseMany
  consumeSpaces
  consumeCErr ';'
  pure $ ADefn name body

parse :: String -> Either String [AST]
parse s = case parsed of
  Left Nothing -> Left "Parse error"
  Left (Just e) -> Left e
  Right ast -> Right ast
  where
    parsed = do
      (ast, rest) <- runParser parseMany s
      case rest of
        [] -> Right ast
        _ -> parseErr $ "Incomplete parse: " ++ rest

parseMany :: Parser [AST]
parseMany = do
  next <- parseOne
  consumeSpaces
  rest <- parseMany <|> pure []
  pure $ next : rest

parseOne :: Parser AST
parseOne = do
  consumeSpaces
  next <- parseComm <|> parseDef <|> parseLam <|> parseInt <|> parseCall
  pure next

parseComm :: Parser AST
parseComm = do
  consumeC '('
  v <- parseWhile (/= ')')
  consumeCErr ')'
  pure $ AComm v

parseLam :: Parser AST
parseLam = do
  consumeC '['
  consumeSpaces
  body <- parseMany
  consumeSpaces
  consumeCErr ']'
  pure $ ALamb body

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = Parser $ \s -> Right $ span p s

parseCall :: Parser AST
parseCall = do
  id <- parseIdentS
  pure $ ACall id

parseInt :: Parser AST
parseInt = do
  num <- parseWhile isDigit
  case readMaybe num of
    Just i -> pure $ AIntL i
    Nothing -> empty

parseIdentS :: Parser String
parseIdentS = do
  consumeSpaces
  i <- parseWhile (not . isSpace)
  case i of
    "" -> errParser "Expected identifier"
    [c] | c `elem` ":;[]" -> errParser $ "Invalid identifier '" ++ [c] ++ "'"
    _ -> pure i

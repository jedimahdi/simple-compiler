module Main where

import Control.Applicative
import Control.Monad.State
import Data.Char
import qualified Data.Map.Strict as Map
import Parser

newtype Variable =
  Variable String
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  deriving (Show, Eq)

data ValueType
  = Var Variable
  | Number Int
  deriving (Eq, Show)

data Expr
  = Value ValueType
  | Compute Op Expr Expr
  deriving (Show, Eq)

var :: Parser ValueType
var =
  Var . Variable <$>
  (ws *> some (sat (\x -> not (isSpace x) && x /= '+' && x /= '-')) <* ws)

number :: Parser ValueType
number = Number <$> (ws *> int <* ws)

valueP :: Parser Expr
valueP = Value <$> (number <|> var)

operator :: Char -> Maybe Expr -> (Expr -> Expr -> Expr) -> Parser Expr
operator c Nothing f = do
  ol <- valueP
  _ <- ws *> charP c <* ws
  or <- valueP
  pure $ f ol or
operator c (Just expr) f = do
  _ <- ws *> charP c <* ws
  or <- valueP
  pure $ f expr or

compute :: Maybe Expr -> Parser Expr
compute Nothing = do
  (ex, isEnded) <-
    isParsed
      (operator '+' Nothing (Compute Add) <|> operator '-' Nothing (Compute Sub))
  if isEnded
    then pure ex
    else compute (Just ex)
compute (Just expr) = do
  (ex, isEnded) <-
    isParsed
      (operator '+' (Just expr) (Compute Add) <|>
       operator '-' (Just expr) (Compute Sub))
  if isEnded
    then pure ex
    else compute (Just ex)

expression :: Parser Expr
expression = fullParsed (compute Nothing) <|> fullParsed valueP

data Statement
  = InputStatement Variable
  | OutputStatement Variable
  | ReplacementStatement Variable Expr
  deriving (Show, Eq)

inputStatement :: Parser Statement
inputStatement =
  InputStatement . Variable <$>
  fullParsed (ws *> charP '?' *> ws *> spanP (/= ' ') <* ws)

outputStatement :: Parser Statement
outputStatement =
  OutputStatement . Variable <$>
  fullParsed (ws *> charP '!' *> ws *> spanP (/= ' ') <* ws)

replacementStatement :: Parser Statement
replacementStatement = do
  var <- ws *> spanP (\x -> x /= ' ' && x /= '=') <* ws <* charP '='
  expr <- expression
  pure $ ReplacementStatement (Variable var) expr

statement :: Parser Statement
statement = inputStatement <|> outputStatement <|> replacementStatement

type Vars = Map.Map String Int

evalExpr :: Expr -> Vars -> Int
evalExpr (Value (Var (Variable x))) vars =
  case Map.lookup x vars of
    Just v -> v
    Nothing -> undefined
evalExpr (Value (Number x)) _ = x
evalExpr (Compute Add l r) vars = evalExpr l vars + evalExpr r vars
evalExpr (Compute Sub l r) vars = evalExpr l vars - evalExpr r vars

eval :: Statement -> StateT Vars IO ()
eval (InputStatement (Variable x)) = do
  vars <- get
  y <- liftIO getLine
  put $ Map.insert x (read y) vars
eval (OutputStatement (Variable x)) = do
  vars <- get
  case Map.lookup x vars of
    Just v -> liftIO $ print v
    Nothing -> undefined
  return ()
eval (ReplacementStatement (Variable x) expression) = do
  vars <- get
  let resultExpr = evalExpr expression vars
  case Map.lookup x vars of
    Just v -> put $ Map.adjust (const resultExpr) x vars
    Nothing -> put $ Map.insert x resultExpr vars
  return ()

parseFile' :: FilePath -> Parser a -> IO (Maybe [a])
parseFile' fileName parser = do
  input <- readFile fileName
  let linesOfFile = lines input
  return (traverse (fmap snd . runParser parser) linesOfFile)

parseFile :: FilePath -> Parser Statement -> StateT Vars IO ()
parseFile fileName parser = do
  input <- liftIO $ readFile fileName
  let linesOfFile = lines input
  let mps = traverse (fmap snd . runParser parser) linesOfFile
  case mps of
    Just ps -> evalStatements ps
    Nothing -> undefined

evalStatements :: [Statement] -> StateT Vars IO ()
evalStatements = mapM_ eval

main :: IO ()
main = do
  runStateT (parseFile "./input" statement) Map.empty
  pure ()

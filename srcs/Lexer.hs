module Lexer (Token (..), lexer) where

import Data.Char (isAlpha, isDigit, isSpace)

data Token
  = TokNum Double (Int, Int) -- 数値
  | TokIdent String (Int, Int) -- 識別子
  | TokEqual (Int, Int) -- '='
  | TokPlus (Int, Int) -- '+'
  | TokMinus (Int, Int) -- '-'
  | TokMul (Int, Int) -- '*'
  | TokPow (Int, Int) -- '^'
  | TokLParen (Int, Int) -- '('
  | TokRParen (Int, Int) -- ')'
  deriving (Show, Eq)

lexer :: Int -> String -> (Int, [Token])
lexer i [] = (i, [])
lexer i (c : cs)
  | isSpace c = lexer (i + 1) cs -- 空白は無視
  | isDigit c = lexNum i (c : cs) -- 数値のトークン化
  | isAlpha c = lexIdent i (c : cs) -- 識別子のトークン化
  | c == '=' = let (i', tokens) = lexer (i + 1) cs in (i', TokEqual (i, i + 1) : tokens) -- '=' 演算子
  | c == '+' = let (i', tokens) = lexer (i + 1) cs in (i', TokPlus (i, i + 1) : tokens) -- '+' 演算子
  | c == '-' = let (i', tokens) = lexer (i + 1) cs in (i', TokMinus (i, i + 1) : tokens) -- '-' 演算子
  | c == '*' = let (i', tokens) = lexer (i + 1) cs in (i', TokMul (i, i + 1) : tokens) -- '*' 演算子
  | c == '^' = let (i', tokens) = lexer (i + 1) cs in (i', TokPow (i, i + 1) : tokens) -- '^' 演算子
  | c == '(' = let (i', tokens) = lexer (i + 1) cs in (i', TokLParen (i, i + 1) : tokens) -- '(' 演算子
  | c == ')' = let (i', tokens) = lexer (i + 1) cs in (i', TokRParen (i, i + 1) : tokens) -- ')' 演算子
  | otherwise = error $ "Unexpected character: " ++ [c]

-- 数字を解析するヘルパー関数
lexNum :: Int -> String -> (Int, [Token])
lexNum i cs = (i'', TokNum (read numStr) (i, i') : tokens)
  where
    (numStr, rest) = span (\c -> isDigit c || c == '.') cs
    i' = i + length numStr
    (i'', tokens) = lexer i' rest

-- 識別子を解析するヘルパー関数
lexIdent :: Int -> String -> (Int, [Token])
lexIdent i cs = (i'', TokIdent identStr (i, i') : tokens)
  where
    (identStr, rest) = span isAlpha cs
    i' = i + length identStr
    (i'', tokens) = lexer i' rest

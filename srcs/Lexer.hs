module Lexer (Token (..), lexer, tokenRange) where

import Data.Char (isAlpha, isDigit, isSpace)
import MyPrint

type TokenRange = (Int, Int)

data Token
  = TokNum Double TokenRange -- 数値
  | TokIdent String TokenRange -- 識別子
  | TokEqual TokenRange -- '='
  | TokPlus TokenRange -- '+'
  | TokMinus TokenRange -- '-'
  | TokMul TokenRange -- '*'
  | TokDiv TokenRange -- '*'
  | TokPow TokenRange -- '^'
  | TokLParen TokenRange -- '('
  | TokRParen TokenRange -- ')'
  deriving (Show, Eq)

data Context = Context
  { expression :: String,
    index :: Int
  }
  deriving (Show)

nc :: Context -> Int -> Context
nc ctx n = ctx {index = index ctx + n}

sayError :: Context -> String -> a
sayError ctx msg = do
  let i = index ctx
  let emphasized = MyPrint.emphasis (expression ctx) (i, i + 1)
  error $ "TokenizeError: " ++ msg ++ "\n" ++ emphasized

tokenRange :: Token -> TokenRange
tokenRange (TokNum _ r) = r
tokenRange (TokIdent _ r) = r
tokenRange (TokEqual r) = r
tokenRange (TokPlus r) = r
tokenRange (TokMinus r) = r
tokenRange (TokMul r) = r
tokenRange (TokDiv r) = r
tokenRange (TokPow r) = r
tokenRange (TokLParen r) = r
tokenRange (TokRParen r) = r

lexer :: String -> [Token]
lexer cs = let (_, tokens) = lexer_ (Context {expression = cs, index = 0}) cs in tokens

lexer_ :: Context -> String -> (Context, [Token])
lexer_ ctx [] = (ctx, [])
lexer_ ctx (c : cs)
  | isSpace c = lexer_ (nc ctx 1) cs -- 空白は無視
  | isDigit c = lexNum ctx (c : cs) -- 数値のトークン化
  | isAlpha c = lexIdent ctx (c : cs) -- 識別子のトークン化
  | c == '=' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokEqual (i, i + 1) : tokens) -- '=' 演算子
  | c == '+' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokPlus (i, i + 1) : tokens) -- '+' 演算子
  | c == '-' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokMinus (i, i + 1) : tokens) -- '-' 演算子
  | c == '*' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokMul (i, i + 1) : tokens) -- '*' 演算子
  | c == '/' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokDiv (i, i + 1) : tokens) -- '/' 演算子
  | c == '^' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokPow (i, i + 1) : tokens) -- '^' 演算子
  | c == '(' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokLParen (i, i + 1) : tokens) -- '(' 演算子
  | c == ')' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokRParen (i, i + 1) : tokens) -- ')' 演算子
  | otherwise = sayError ctx $ "Unexpected character: " ++ [c]
  where
    i = index ctx

-- 数字を解析するヘルパー関数
lexNum :: Context -> String -> (Context, [Token])
lexNum ctx cs = (ctx'', TokNum (read numStr) (index ctx, index ctx') : tokens)
  where
    (numStr, rest) = span (\c -> isDigit c || c == '.') cs
    ctx' = nc ctx (length numStr)
    (ctx'', tokens) = lexer_ ctx' rest

-- 識別子を解析するヘルパー関数
lexIdent :: Context -> String -> (Context, [Token])
lexIdent ctx cs = (ctx'', TokIdent identStr (index ctx, index ctx') : tokens)
  where
    (identStr, rest) = span isAlpha cs
    ctx' = nc ctx (length identStr)
    (ctx'', tokens) = lexer_ ctx' rest

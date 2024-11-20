module Lexer (Token (..), lexer, tokenRange) where

import qualified Data.Text as T
import Data.Char (isAlpha, isDigit, isSpace)
import MyPrint
import Debug.Trace (trace)

type TokenRange = (Int, Int)

data Token
  = TokNum Double TokenRange -- 数値
  | TokIdent T.Text TokenRange -- 識別子
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
  { expression :: T.Text,
    idx :: Int
  }
  deriving (Show)

nc :: Context -> Int -> Context
nc ctx n = ctx {idx = idx ctx + n}

sayError :: Context -> T.Text -> a
sayError ctx msg = do
  let i = idx ctx
  let emphasized = MyPrint.emphasis (expression ctx) (i, i + 1)
  error $ T.unpack $ T.concat [T.pack "TokenizeError: ", msg, T.pack "\n", emphasized]

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

lexer :: T.Text -> [Token]
lexer cs = let (_, tokens) = lexer_ (Context {expression = cs, idx = 0}) cs in tokens

lexer_ :: Context -> T.Text -> (Context, [Token])
lexer_ ctx txt = case T.uncons txt of
  Nothing -> (ctx, [])
  Just (c, cs) -> lexer__ ctx (c, cs, txt)
    where
      i = idx ctx

      -- 数字を解析するヘルパー関数
      lexNum :: Context -> T.Text -> (Context, [Token])
      lexNum ctx cs = (ctx'', TokNum (read(T.unpack numStr)) (idx ctx, idx ctx') : tokens)
        where
          (numStr, rest) = T.span (\c -> isDigit c || c == '.') cs
          ctx' = nc ctx (T.length numStr)
          (ctx'', tokens) = lexer_ ctx' rest

      -- 識別子を解析するヘルパー関数
      lexIdent :: Context -> T.Text -> (Context, [Token])
      lexIdent ctx cs = (ctx'', TokIdent identStr (idx ctx, idx ctx') : tokens)
        where
          (identStr, rest) = T.span isVarChar cs
          ctx' = nc ctx (T.length identStr)
          (ctx'', tokens) = lexer_ ctx' rest

      isSymbol :: Char -> Bool
      isSymbol c = c `elem` ['=', '+', '-', '*', '/', '^', '(', ')']

      isVarChar :: Char -> Bool
      isVarChar c = not (isSpace c || isDigit c || isSymbol c)

      lexer__ :: Context -> (Char, T.Text, T.Text) -> (Context, [Token])
      lexer__ ctx (c, cs, txt)
        | isSpace c = lexer_ (nc ctx 1) cs -- 空白は無視
        | isVarChar c = lexIdent ctx txt -- 識別子のトークン化
        | isDigit c = lexNum ctx txt -- 数値のトークン化
        | c == '=' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokEqual (i, i + 1) : tokens) -- '=' 演算子
        | c == '+' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokPlus (i, i + 1) : tokens) -- '+' 演算子
        | c == '-' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokMinus (i, i + 1) : tokens) -- '-' 演算子
        | c == '*' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokMul (i, i + 1) : tokens) -- '*' 演算子
        | c == '/' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokDiv (i, i + 1) : tokens) -- '/' 演算子
        | c == '^' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokPow (i, i + 1) : tokens) -- '^' 演算子
        | c == '(' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokLParen (i, i + 1) : tokens) -- '(' 演算子
        | c == ')' = let (ctx', tokens) = lexer_ (nc ctx 1) cs in (ctx', TokRParen (i, i + 1) : tokens) -- ')' 演算子
        -- | otherwise = sayError ctx $ T.pack $ "Unexpected character: " ++ [c]

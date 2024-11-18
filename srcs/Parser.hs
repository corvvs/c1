module Parser (parseEquation, AST (..), Equation (..)) where

import AST (AST (..), makeMul, reduceMulDiv)
import Debug.Trace (trace)
import Lexer (Token (..), lexer)

-- 方程式全体を表すデータ型
data Equation = Equation AST AST
  deriving (Show, Eq)

-- パーサー：方程式全体を解析
parseEquation :: [Token] -> Equation
parseEquation [] = error "Parse Error: no tokens"
parseEquation tokens = case break isEqual tokens of
  ([], _) -> error "Parse Error: Missing LHS"
  (_, [TokEqual _]) -> error "Parse Error: Missing RHS"
  (lhsTokens, TokEqual _ : rhsTokens) ->
    let lhs = parseExpr lhsTokens
        rhs = parseExpr rhsTokens
     in Equation lhs rhs
  (_, []) -> error "Parse Error: Missing \"=\""
  _ -> error "Invalid equation format"
  where
    isEqual :: Token -> Bool
    isEqual (TokEqual _) = True
    isEqual _ = False

-- 式を解析
parseExpr :: [Token] -> AST
parseExpr tokens =
  let (n, ast, rest) = parseAddSub 0 tokens
   in if trace ("n = " ++ show n) null rest
        then ast
        else error "Unexpected token sequence"

-- 加算・減算を解析
parseAddSub :: Int -> [Token] -> (Int, AST, [Token])
parseAddSub i tokens =
  let (i', term, rest) = parseMulDiv i tokens
   in parseAddSub' i' term rest

-- 二項+, 二項-を解析
parseAddSub' :: Int -> AST -> [Token] -> (Int, AST, [Token])
parseAddSub' i ast (TokPlus _ : tokens) =
  let (i', term, rest) = parseMulDiv (i + 1) tokens
   in parseAddSub' i' (Add ast term) rest
parseAddSub' i ast (TokMinus _ : tokens) =
  let (i', term, rest) = parseMulDiv (i + 1) tokens
   in parseAddSub' i' (Sub ast term) rest
parseAddSub' i ast tokens = (i, ast, tokens)

-- 乗算を解析
parseMulDiv :: Int -> [Token] -> (Int, AST, [Token])
parseMulDiv i tokens =
  let (i', factor, rest) = parsePow i tokens
   in parseMulDiv' i' factor rest

parseMulDiv' :: Int -> AST -> [Token] -> (Int, AST, [Token])
parseMulDiv' i ast (TokMul _ : tokens) =
  let (i', factor, rest) = parsePow (i + 1) tokens
   in parseMulDiv' i' (Mul ast factor) rest
parseMulDiv' i ast tokens = (i, ast, tokens)

-- 累乗を解析
parsePow :: Int -> [Token] -> (Int, AST, [Token])
parsePow i tokens =
  let (i', base, rest) = parseUnary i tokens
   in parsePow' i' base rest

parsePow' :: Int -> AST -> [Token] -> (Int, AST, [Token])
parsePow' i base (TokPow _ : TokNum x pos : rest) =
  let (i', exponent, _) = parseTerm (i + 1) [TokNum x pos]
   in (i', Pow base exponent, rest)
parsePow' i base tokens = (i, base, tokens)

parseUnary :: Int -> [Token] -> (Int, AST, [Token])
-- 単項プラス
parseUnary i (TokPlus _ : tokens) = parseParen (i + 1) tokens
-- 単項マイナス
parseUnary i (TokMinus _ : tokens) =
  let (i', term, rest) = parseParen (i + 1) tokens
   in (i', Mul (Num (-1.0)) term, rest)
-- フォールスルー
parseUnary i tokens = parseParen i tokens

-- 括弧を解析

parseParen :: Int -> [Token] -> (Int, AST, [Token])
parseParen i (TokLParen _ : tokens) =
  let (i', subexpr, rest) = parseAddSub (i + 1) tokens
   in case rest of
        (TokRParen _ : tokens) -> (i' + 1, subexpr, tokens)
        _ -> error "Parse Error: Missing closing parenthesis"
parseParen i tokens = parseTerm i tokens

-- 単項（数値や変数）を解析
parseTerm :: Int -> [Token] -> (Int, AST, [Token])
-- 数値
parseTerm i (TokNum n _ : tokens) = (i + 1, Num n, tokens)
-- 変数（X ^ n）
parseTerm i (TokIdent var _ : TokPow _ : TokNum exp _ : tokens) =
  if exp == 0
    then (i + 3, Num 1, tokens)
    else (i + 3, Var var (round exp), tokens)
-- 変数（X）
parseTerm i (TokIdent var _ : tokens) =
  (i + 1, Var var 1, tokens)
parseTerm _ (t : _) = error $ "Parse Error: Unexpected Token in this Context: " ++ show t
parseTerm _ [] = error "Parse Error: Neither Number nor Variable Token"

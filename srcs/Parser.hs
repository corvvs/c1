module Parser (parseEquation, AST (..), Equation (..)) where

import AST (AST (..), makeMul, reduceMulDiv)
import Debug.Trace (trace)
import Lexer (Token (..), lexer)

-- 方程式全体を表すデータ型
data Equation = Equation AST AST
  deriving (Show, Eq)

-- パーサー：方程式全体を解析
parseEquation :: [Token] -> Equation
parseEquation tokens = case break (== TokEqual) tokens of
  (lhsTokens, TokEqual : rhsTokens) ->
    let lhs = parseExpr lhsTokens
        rhs = parseExpr rhsTokens
     in Equation lhs rhs
  _ -> error "Invalid equation format"

-- 式を解析
parseExpr :: [Token] -> AST
parseExpr tokens =
  let (ast, rest) = parseAddSub tokens
   in if null rest
        then ast
        else error "Unexpected token sequence"

-- 加算・減算を解析
parseAddSub :: [Token] -> (AST, [Token])
parseAddSub tokens =
  let (term, rest) = parseMulDiv tokens
   in parseAddSub' term rest

-- 二項+, 二項-を解析
parseAddSub' :: AST -> [Token] -> (AST, [Token])
parseAddSub' ast (TokPlus : tokens) =
  let (term, rest) = parseMulDiv tokens
   in parseAddSub' (Add ast term) rest
parseAddSub' ast (TokMinus : tokens) =
  let (term, rest) = parseMulDiv tokens
   in parseAddSub' (Sub ast term) rest
parseAddSub' ast tokens = (ast, tokens)

-- 乗算を解析
parseMulDiv :: [Token] -> (AST, [Token])
parseMulDiv tokens =
  let (factor, rest) = parsePow tokens
   in parseMulDiv' factor rest

parseMulDiv' :: AST -> [Token] -> (AST, [Token])
parseMulDiv' ast (TokMul : tokens) =
  let (factor, rest) = parsePow tokens
   in parseMulDiv' (Mul ast factor) rest
parseMulDiv' ast tokens = (ast, tokens)

-- 累乗を解析
parsePow :: [Token] -> (AST, [Token])
parsePow tokens =
  let (base, rest) = parseUnary tokens
   in parsePow' base rest

parsePow' :: AST -> [Token] -> (AST, [Token])
parsePow' base (TokPow : TokNum x : rest) =
  let (exponent, _) = parseTerm [TokNum x]
   in (Pow base exponent, rest)
parsePow' base tokens = (base, tokens)

parseUnary :: [Token] -> (AST, [Token])
-- 単項プラス
parseUnary (TokPlus : tokens) = parseParen tokens
-- 単項マイナス
parseUnary (TokMinus : tokens) = let (term, rest) = parseParen tokens in (Mul (Num (-1.0)) term, rest)
-- フォールスルー
parseUnary tokens = parseParen tokens

-- 括弧を解析

parseParen :: [Token] -> (AST, [Token])
parseParen (TokLParen : tokens) =
  let (subexpr, rest) = parseAddSub tokens
   in case rest of
        (TokRParen : tokens) -> (subexpr, tokens)
        _ -> error "Missing closing parenthesis"
parseParen tokens = parseTerm tokens

-- 単項（数値や変数）を解析
parseTerm :: [Token] -> (AST, [Token])
-- 数値
parseTerm (TokNum n : tokens) = (Num n, tokens)
-- 変数（X ^ n）
parseTerm (TokIdent var : TokPow : TokNum exp : tokens) =
  if exp == 0
    then (Num 1, tokens)
    else (Var var (round exp), tokens)
-- 変数（X）
parseTerm (TokIdent var : tokens) =
  (Var var 1, tokens)
parseTerm tokens = error $ "Unexpected token sequence: " ++ show tokens

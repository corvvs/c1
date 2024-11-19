module Parser (parseEquation, AST (..), Equation (..)) where

import AST (AST (..))
import Debug.Trace (trace)
import Lexer (Token (..), lexer, tokenRange)
import MyPrint

-- 方程式全体を表すデータ型
data Equation = Equation AST AST
  deriving (Show, Eq)

data ParseContext = ParseContext
  { expression :: String,
    tokens :: [Token],
    index :: Int
  }
  deriving (Show)

nc :: ParseContext -> Int -> ParseContext
nc ctx n = let i = index ctx in ctx {index = i + n}

sayError :: ParseContext -> String -> a
sayError ctx msg =
  error $ "ParseError: " ++ combinedMsg
  where
    combineMsg :: ParseContext -> String -> String
    combineMsg ctx msg = msg ++ "\n" ++ emphasized ++ "\n"
      where
        ts = tokens ctx
        i = index ctx - 1
        t = ts !! i
        range = tokenRange t
        emphasized = MyPrint.emphasis (expression ctx) range
    combinedMsg =
      if index ctx == 0
        then msg
        else combineMsg ctx msg

-- パーサー：方程式全体を解析
parseEquation :: String -> [Token] -> Equation
parseEquation expr ts =
  let ctx = ParseContext {expression = expr, tokens = ts, index = 0}
   in parseEquation' ctx ts

parseEquation' :: ParseContext -> [Token] -> Equation
parseEquation' ctx [] = sayError ctx "no tokens"
parseEquation' ctx tokens = case break isEqual tokens of
  ([], _) -> sayError ctx "Missing LHS"
  (_, [TokEqual _]) -> sayError ctx "Missing RHS"
  (lhsTokens, TokEqual _ : rhsTokens) -> Equation lhs rhs
    where
      (ctx', lhs) = parseExpr ctx lhsTokens
      (_, rhs) = parseExpr (nc ctx' 1) rhsTokens
  (_, []) -> sayError ctx "Missing \"=\""
  _ -> sayError ctx "Invalid equation format"
  where
    isEqual :: Token -> Bool
    isEqual (TokEqual _) = True
    isEqual _ = False

-- 式を解析
parseExpr :: ParseContext -> [Token] -> (ParseContext, AST)
parseExpr ctx tokens =
  let (ctx', ast, rest) = parseAddSub ctx tokens
   in if null rest
        then (ctx', ast)
        else sayError (nc ctx' 1) "Unexpected Extra Token(s)"

-- 加算・減算を解析
parseAddSub :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseAddSub ctx tokens =
  let (ctx', term, rest) = parseMulDiv ctx tokens
   in parseAddSub' ctx' term rest

-- 二項+, 二項-を解析
parseAddSub' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parseAddSub' ctx ast (TokPlus _ : tokens) =
  let (ctx', term, rest) = parseMulDiv (nc ctx 1) tokens
   in parseAddSub' ctx' (Add ast term) rest
parseAddSub' ctx ast (TokMinus _ : tokens) =
  let (ctx', term, rest) = parseMulDiv (nc ctx 1) tokens
   in parseAddSub' ctx' (Sub ast term) rest
parseAddSub' ctx ast tokens = (ctx, ast, tokens)

-- 乗算を解析
parseMulDiv :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseMulDiv ctx tokens =
  let (ctx', factor, rest) = parsePow ctx tokens
   in parseMulDiv' ctx' factor rest

parseMulDiv' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parseMulDiv' ctx ast (TokMul _ : tokens) =
  let (ctx', factor, rest) = parsePow (nc ctx 1) tokens
   in parseMulDiv' ctx' (Mul ast factor) rest
parseMulDiv' ctx ast (TokDiv _ : tokens) =
  let (ctx', factor, rest) = parsePow (nc ctx 1) tokens
   in parseMulDiv' ctx' (Div ast factor) rest
parseMulDiv' ctx ast tokens = (ctx, ast, tokens)

-- 累乗を解析
parsePow :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parsePow ctx tokens =
  let (ctx', base, rest) = parseUnary ctx tokens
   in parsePow' ctx' base rest

parsePow' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parsePow' ctx base (TokPow _ : TokNum x pos : rest) =
  let (ctx', exponent, _) = parseTerm (nc ctx 1) [TokNum x pos]
   in (ctx', Pow base exponent, rest)
parsePow' ctx base (TokPow _ : _) = sayError (nc ctx 1) "Power(^) requires Number as Right Operand"
parsePow' ctx base tokens = (ctx, base, tokens)

parseUnary :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
-- 単項プラス
parseUnary ctx (TokPlus _ : tokens) = parseParen (nc ctx 1) tokens
-- 単項マイナス
parseUnary ctx (TokMinus _ : tokens) =
  let (ctx', term, rest) = parseParen (nc ctx 1) tokens
   in (ctx', Mul (Num (-1.0)) term, rest)
-- フォールスルー
parseUnary ctx tokens = parseParen ctx tokens

-- 括弧を解析
parseParen :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseParen ctx (TokLParen _ : tokens) =
  let ctx' = nc ctx 1
   in let (ctx'', subexpr, rest) = parseAddSub ctx' tokens
       in case rest of
            (TokRParen _ : tokens) -> (nc ctx'' 1, subexpr, tokens)
            _ -> sayError ctx' "Missing Closing Parenthesis(')') in Right Position"
parseParen ctx tokens = parseTerm ctx tokens

-- 単項（数値や変数）を解析
parseTerm :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
-- 数値
parseTerm ctx (TokNum n _ : tokens) = (nc ctx 1, Num n, tokens)
-- 変数（X ^ n）
parseTerm ctx (TokIdent var _ : TokPow _ : TokNum exp _ : tokens) =
  if exp == 0
    then (nc ctx 3, Num 1, tokens)
    else (nc ctx 3, Var var (round exp), tokens)
-- 変数（X）
parseTerm ctx (TokIdent var _ : tokens) =
  (nc ctx 1, Var var 1, tokens)
  
-- まずいパターン
parseTerm ctx (TokRParen _ : _) = sayError (nc ctx 1) "Unexpected Closing Parenthesis(')')"
parseTerm ctx (TokPow _ : _) = sayError (nc ctx 1) "Power(^) requires Left Operand"
parseTerm ctx (t : _) = sayError ctx $ "Unexpected Token in this ParseContext: " ++ show t
parseTerm ctx [] = sayError ctx "Neither Number nor Variable Token"

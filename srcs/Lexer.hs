module Lexer (Token, lexer, parseEquation) where
import Data.Char (isDigit, isAlpha, isSpace)

data Token
    = TokNum Double     -- 数値
    | TokIdent String   -- 識別子
    | TokEqual          -- '='
    | TokPlus           -- '+'
    | TokMinus          -- '-'
    | TokMul            -- '*'
    | TokPow            -- '^'
    | TokEOF            -- ファイルの終端
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs                                -- 空白は無視
    | isDigit c = lexNum (c:cs)                           -- 数値のトークン化
    | isAlpha c = lexIdent (c:cs)                         -- 識別子のトークン化
    | c == '=' = TokEqual : lexer cs                      -- '=' 演算子
    | c == '+' = TokPlus : lexer cs                       -- '+' 演算子
    | c == '-' = TokMinus : lexer cs                      -- '-' 演算子
    | c == '*' = TokMul : lexer cs                        -- '*' 演算子
    | c == '^' = TokPow : lexer cs                        -- '^' 演算子
    | otherwise = error $ "Unexpected character: " ++ [c]


-- 数字を解析するヘルパー関数
lexNum :: String -> [Token]
lexNum cs = TokNum (read numStr) : lexer rest
  where (numStr, rest) = span (\c -> isDigit c || c == '.') cs

-- 識別子を解析するヘルパー関数
lexIdent :: String -> [Token]
lexIdent cs = TokIdent identStr : lexer rest
  where (identStr, rest) = span isAlpha cs


-- 構文木のデータ型
data AST
    = Num Double           -- 数値
    | Var String Int       -- 変数（名前と指数）
    | Add AST AST          -- 加算
    | Sub AST AST          -- 減算
    | Mul AST AST          -- 乗算
    | Pow AST AST          -- 累乗
    deriving (Show, Eq)

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
parseExpr = parseAddSub

-- 加算・減算を解析
parseAddSub :: [Token] -> AST
parseAddSub tokens =
    let (term, rest) = parseMulDiv tokens
      in parseAddSub' term rest

parseAddSub' :: AST -> [Token] -> AST
parseAddSub' ast (TokPlus : tokens) =
    let (term, rest) = parseMulDiv tokens
      in parseAddSub' (Add ast term) rest
parseAddSub' ast (TokMinus : tokens) =
    let (term, rest) = parseMulDiv tokens
      in parseAddSub' (Sub ast term) rest
parseAddSub' ast [] = ast
parseAddSub' ast tokens = error "Unexpected token sequence"

-- 乗算を解析
parseMulDiv :: [Token] -> (AST, [Token])
parseMulDiv tokens =
    let (factor, rest) = parseFactor tokens
      in parseMulDiv' factor rest

parseMulDiv' :: AST -> [Token] -> (AST, [Token])
parseMulDiv' ast (TokMul : tokens) =
    let (factor, rest) = parseFactor tokens
      in parseMulDiv' (Mul ast factor) rest
parseMulDiv' ast tokens = (ast, tokens)

-- 累乗を解析
parseFactor :: [Token] -> (AST, [Token])
parseFactor tokens =
    let (base, rest) = parseTerm tokens
      in parsePow base rest

parsePow :: AST -> [Token] -> (AST, [Token])
parsePow base (TokPow : tokens) =
    let (exponent, rest) = parseTerm tokens
      in (Pow base exponent, rest)
parsePow base tokens = (base, tokens)

-- 単項（数値や変数）を解析
parseTerm :: [Token] -> (AST, [Token])
-- 数値
parseTerm (TokNum n : tokens) = (Num n, tokens)
-- 変数（X ^ n）
parseTerm (TokIdent var : TokPow : TokNum exp : tokens) =
    (Var var (round exp), tokens)
-- 変数（X）
parseTerm (TokIdent var : tokens) =
    (Var var 1, tokens)

parseTerm tokens = error $ "Unexpected token sequence: " ++ show tokens

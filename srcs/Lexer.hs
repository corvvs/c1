module Lexer (Token, lexer) where
import Data.Char (isDigit, isAlpha, isSpace)

data Token
    = TokNum Double     -- 数値
    | TokIdent String   -- 識別子
    | TokEqual          -- '='
    | TokPlus           -- '+'
    | TokMinus          -- '-'
    | TokMul            -- '*'
    | TokPow            -- '^'
    | TokLParen         -- '('
    | TokRParen         -- ')'
    | TokEOF            -- ファイルの終端
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = [TokEOF]
lexer (c:cs)
    | isSpace c = lexer cs                                -- 空白は無視
    | isDigit c = lexNum (c:cs)                           -- 数値のトークン化
    | isAlpha c = lexIdent (c:cs)                         -- 識別子のトークン化
    | c == '=' = TokEqual : lexer cs                      -- '=' 演算子
    | c == '+' = TokPlus : lexer cs                       -- '+' 演算子
    | c == '-' = TokMinus : lexer cs                      -- '-' 演算子
    | c == '*' = TokMul : lexer cs                        -- '*' 演算子
    | c == '^' = TokPow : lexer cs                        -- '^' 演算子
    | c == '(' = TokLParen : lexer cs                     -- '(' 括弧
    | c == ')' = TokRParen : lexer cs                     -- ')' 括弧
    | otherwise = error $ "Unexpected character: " ++ [c]


-- 数字を解析するヘルパー関数
lexNum :: String -> [Token]
lexNum cs = TokNum (read numStr) : lexer rest
  where (numStr, rest) = span (\c -> isDigit c || c == '.') cs

-- 識別子を解析するヘルパー関数
lexIdent :: String -> [Token]
lexIdent cs = TokIdent identStr : lexer rest
  where (identStr, rest) = span isAlpha cs
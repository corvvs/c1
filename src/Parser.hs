module Parser (parseEquation, AST (..), Equation (..)) where

import Control.Monad.Except
import qualified Data.Text as T
import AST (AST (..))
import Lexer (Token (..), tokenRange)
import MyPrint

-- 方程式全体を表すデータ型
data Equation = Equation AST AST
  deriving (Show, Eq)

data ParseContext = ParseContext
  { expression :: T.Text,
    tokens :: [Token],
    idx :: Int
  }
  deriving (Show)

nc :: ParseContext -> Int -> ParseContext
nc ctx n = let i = idx ctx in ctx {idx = i + n}

sayError :: ParseContext -> String -> a
sayError ctx msg =
  error $ T.unpack $ T.concat [T.pack "ParseError: ", combinedMsg]
  where
    combineMsg :: ParseContext -> String -> T.Text
    combineMsg ctx' msg' = T.concat [T.pack msg', T.pack "\n", emphasized, T.pack "\n"]
      where
        ts = tokens ctx'
        i = idx ctx' - 1
        t = ts !! i
        range = tokenRange t
        emphasized = MyPrint.emphasis (expression ctx') range
    combinedMsg =
      if idx ctx == 0
        then T.pack msg
        else combineMsg ctx msg

-- パーサー：方程式全体を解析
parseEquation :: T.Text -> [Token] -> ExceptT T.Text IO Equation
parseEquation expr ts =
  let ctx = ParseContext {expression = expr, tokens = ts, idx = 0}
   in return $ parseEquation' ctx ts

parseEquation' :: ParseContext -> [Token] -> Equation
parseEquation' ctx [] = sayError ctx "no tokens"
parseEquation' ctx ts = case Prelude.break isEqual ts of
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
parseExpr ctx ts =
  let (ctx', ast, rest) = parseAddSub ctx ts
   in if Prelude.null rest
        then (ctx', ast)
        else sayError (nc ctx' 1) "Unexpected Extra Token(s)"

-- 加算・減算を解析
parseAddSub :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseAddSub ctx ts =
  let (ctx', term, rest) = parseMulDiv ctx ts
   in parseAddSub' ctx' term rest

-- 二項+, 二項-を解析
parseAddSub' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parseAddSub' ctx ast (TokPlus _ : ts) =
  let (ctx', term, rest) = parseMulDiv (nc ctx 1) ts
   in parseAddSub' ctx' (Add ast term) rest
parseAddSub' ctx ast (TokMinus _ : ts) =
  let (ctx', term, rest) = parseMulDiv (nc ctx 1) ts
   in parseAddSub' ctx' (Sub ast term) rest
parseAddSub' ctx ast ts = (ctx, ast, ts)

-- 乗算を解析
parseMulDiv :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseMulDiv ctx ts =
  let (ctx', factor, rest) = parsePow ctx ts
   in parseMulDiv' ctx' factor rest

parseMulDiv' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parseMulDiv' ctx ast (TokMul _ : ts) =
  let (ctx', factor, rest) = parsePow (nc ctx 1) ts
   in parseMulDiv' ctx' (Mul ast factor) rest
parseMulDiv' ctx ast (TokDiv _ : ts) =
  let (ctx', factor, rest) = parsePow (nc ctx 1) ts
   in parseMulDiv' ctx' (Div ast factor) rest
parseMulDiv' ctx ast ts = (ctx, ast, ts)

-- 累乗を解析
parsePow :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parsePow ctx ts =
  let (ctx', base, rest) = parseUnary ctx ts
   in parsePow' ctx' base rest

parsePow' :: ParseContext -> AST -> [Token] -> (ParseContext, AST, [Token])
parsePow' ctx base (TokPow _ : TokNum x pos : rest) =
  let (ctx', expo, _) = parseTerm (nc ctx 1) [TokNum x pos]
   in (ctx', Pow base expo, rest)
parsePow' ctx _ (TokPow _ : _) = sayError (nc ctx 1) "Power(^) requires Number as Right Operand"
parsePow' ctx base totsens = (ctx, base, totsens)

parseUnary :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
-- 単項プラス
parseUnary ctx (TokPlus _ : ts) = parseParen (nc ctx 1) ts
-- 単項マイナス
parseUnary ctx (TokMinus _ : ts) =
  let (ctx', term, rest) = parseParen (nc ctx 1) ts
   in (ctx', Mul (Num (-1.0)) term, rest)
-- フォールスルー
parseUnary ctx ts = parseParen ctx ts

-- 括弧を解析
parseParen :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
parseParen ctx (TokLParen _ : ts) =
  let ctx' = nc ctx 1
   in let (ctx'', subexpr, rest) = parseAddSub ctx' ts
       in case rest of
            (TokRParen _ : ts') -> (nc ctx'' 1, subexpr, ts')
            _ -> sayError ctx' "Missing Closing Parenthesis(')') in Right Position"
parseParen ctx ts = parseTerm ctx ts

-- 単項（数値や変数）を解析
parseTerm :: ParseContext -> [Token] -> (ParseContext, AST, [Token])
-- 数値
parseTerm ctx (TokNum n _ : ts) = (nc ctx 1, Num n, ts)
-- 変数（X ^ n）
parseTerm ctx (TokIdent var _ : TokPow _ : TokNum expo _ : ts) =
  if expo == 0
    then (nc ctx 3, Num 1, ts)
    else (nc ctx 3, Var var (round expo), ts)
-- 変数（X）
parseTerm ctx (TokIdent var _ : ts) =
  (nc ctx 1, Var var 1, ts)
  
-- まずいパターン
parseTerm ctx (TokRParen _ : _) = sayError (nc ctx 1) "Unexpected Closing Parenthesis(')')"
parseTerm ctx (TokPow _ : _) = sayError (nc ctx 1) "Power(^) requires Left Operand"
parseTerm ctx (t : _) = sayError ctx $ "Unexpected Token in this ParseContext: " ++ show t
parseTerm ctx [] = sayError ctx "Neither Number nor Variable Token"

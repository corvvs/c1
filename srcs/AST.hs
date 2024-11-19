module AST(AST(..)) where

-- 構文木のデータ型
data AST
    = Num Double           -- 数値
    | Var String Int       -- 変数（名前と指数）
    | Add AST AST          -- 加算
    | Sub AST AST          -- 減算
    | Mul AST AST          -- 乗算
    | Div AST AST          -- 除算
    | Pow AST AST          -- 累乗
    deriving (Show, Eq)

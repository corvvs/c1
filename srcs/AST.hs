module AST(AST(..), reduceMulDiv, makeMul) where

-- 構文木のデータ型
data AST
    = Num Double           -- 数値
    | Var String Int       -- 変数（名前と指数）
    | Add AST AST          -- 加算
    | Sub AST AST          -- 減算
    | Mul AST AST          -- 乗算
    | Pow AST AST          -- 累乗
    deriving (Show, Eq)

reduceMulDiv :: AST -> AST
reduceMulDiv (Mul (Num a) (Num b)) = Num (a * b)
reduceMulDiv (Mul a (Num b)) = reduceMulDiv (Mul (Num b) (reduceMulDiv a))
reduceMulDiv (Mul (Num 0) b) = Num 0
reduceMulDiv (Mul (Num a) b) = makeMul a (reduceMulDiv b)
reduceMulDiv (Mul a b) = Mul (reduceMulDiv a) (reduceMulDiv b)
reduceMulDiv a = a

makeMul :: Double -> AST -> AST
makeMul 1.0 x = x
makeMul a (Num b) = Num (a * b)
makeMul a (Var n e) = Mul (Num a) (Var n e)
makeMul a (Mul x y) = Mul (makeMul a x) y
makeMul a (Add x y) = Add (makeMul a x) (makeMul a y)
makeMul a (Sub x y) = Sub (makeMul a x) (makeMul a y)
makeMul a (Pow x y) = Mul (Num a) (Pow x y)

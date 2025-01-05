# c1(computorv1)

## about

3次までの1変数 - 実数係数の代数方程式の解を複素数の範囲で求める。

## build

GHC - stackの環境があればそのままビルドできるが、Dockerコンテナの方が楽。

```
$ make up # コンテナをビルド
$ make it # コンテナ内に入る
# make setup # 初回のみ必要かもしれない
# make # コンテナ内で実行ファイルをビルド
```

## usage

```
./computor "[方程式]"
```

## examples

```
# ./computor "X = 1"
[Tokens      ] [TokIdent "X" (0,1),TokEqual (2,3),TokNum 1.0 (4,5)]
[Equation AST] Equation (Var "X" 1) (Num 1.0)
[Reduced form] - 1.0 + X = 0
[Dimension   ] 1
[Solutions   ] + 1.0
```

```
# ./computor "(ほげ + 1) * (ほげ - 1) = -10"
[Tokens      ] [TokLParen (0,1),TokIdent "\12411\12370" (1,3),TokPlus (4,5),TokNum 1.0 (6,7),TokRParen (7,8),TokMul (9,10),TokLParen (11,12),TokIdent "\12411\12370" (12,14),TokMinus (15,16),TokNum 1.0 (17,18),TokRParen (18,19),TokEqual (20,21),TokMinus (22,23),TokNum 10.0 (23,25)]
[Equation AST] Equation (Mul (Add (Var "\12411\12370" 1) (Num 1.0)) (Sub (Var "\12411\12370" 1) (Num 1.0))) (Mul (Num (-1.0)) (Num 10.0))
[Reduced form] 9.0 + ほげ^2 = 0
[Dimension   ] 2
[Discriminant] -36.0(-)
[Solutions   ] +/- 3.0 i
```

```
# ./computor "(🥺+1)^3 = 3"
[Tokens      ] [TokLParen (0,1),TokIdent "\129402" (1,2),TokPlus (2,3),TokNum 1.0 (3,4),TokRParen (4,5),TokPow (5,6),TokNum 3.0 (6,7),TokEqual (8,9),TokNum 3.0 (10,11)]
[Equation AST] Equation (Pow (Add (Var "\129402" 1) (Num 1.0)) (Num 3.0)) (Num 3.0)
[Reduced form] - 2.0 + 3.0*🥺 + 3.0*🥺^2 + 🥺^3 = 0
[Dimension   ] 3
[Discriminant] 405.0(+)
[Solutions   ] 0.4422495703074083 , -1.7211247851537042 + 1.2490247664834064 i, -1.7211247851537035 - 1.2490247664834064 i
```

```
# ./computor "(x + y)^2 = (x - y)^3"
[Tokens      ] [TokLParen (0,1),TokIdent "x" (1,2),TokPlus (3,4),TokIdent "y" (5,6),TokRParen (6,7),TokPow (7,8),TokNum 2.0 (8,9),TokEqual (10,11),TokLParen (12,13),TokIdent "x" (13,14),TokMinus (15,16),TokIdent "y" (17,18),TokRParen (18,19),TokPow (19,20),TokNum 3.0 (20,21)]
[Equation AST] Equation (Pow (Add (Var "x" 1) (Var "y" 1)) (Num 2.0)) (Pow (Sub (Var "x" 1) (Var "y" 1)) (Num 3.0))
[Reduced form] 2.0*x y + x^2 + y^2 - 3.0*x y^2 + 3.0*x^2 y - x^3 + y^3 = 0
[Dimension   ] 3
SolverError: This equation is not solvable. (Too many variables)
```

```
# ./computor "(x + 1)^10 = 0"
[Tokens      ] [TokLParen (0,1),TokIdent "x" (1,2),TokPlus (3,4),TokNum 1.0 (5,6),TokRParen (6,7),TokPow (7,8),TokNum 10.0 (8,10),TokEqual (11,12),TokNum 0.0 (13,14)]
[Equation AST] Equation (Pow (Add (Var "x" 1) (Num 1.0)) (Num 10.0)) (Num 0.0)
[Reduced form] 1.0 + 10.0*x + 45.0*x^2 + 120.0*x^3 + 210.0*x^4 + 252.0*x^5 + 210.0*x^6 + 120.0*x^7 + 45.0*x^8 + 10.0*x^9 + x^10 = 0
[Dimension   ] 10
SolverError: This equation is not solvable. (Too large dimension)
```

```
# ./computor "x^100 = x^100"
[Tokens      ] [TokIdent "x" (0,1),TokPow (1,2),TokNum 100.0 (2,5),TokEqual (6,7),TokIdent "x" (8,9),TokPow (9,10),TokNum 100.0 (10,13)]
[Equation AST] Equation (Var "x" 100) (Var "x" 100)
[Reduced form] 0 = 0
[Dimension   ] 0
[Solutions   ] ARBITRARY COMPLEX NUMBER
```

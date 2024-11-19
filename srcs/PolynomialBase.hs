
module PolynomialBase(PolynomialVariable (..), PolynomialTerm (..), Polynomial (..), polynomialTermByNum, polynomialTermByVar, zeroPolynomial, unitPolynomial, findTerm, degreeOfPolynomial, degreeOfTerm) where

import AST (AST (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Debug.Trace (trace)
import TypeClass (Addable (..), Multipliable (..), Subtractable (..))

-- 変数
type PolynomialVariable = Map.Map String Int

-- 項
data PolynomialTerm = PolynomialTerm Double PolynomialVariable

-- 項のコンストラクタ by Num
polynomialTermByNum :: AST -> PolynomialTerm
polynomialTermByNum (Num a) = PolynomialTerm a Map.empty

-- 項のコンストラクタ by Var
polynomialTermByVar :: AST -> PolynomialTerm
polynomialTermByVar (Var n e) = PolynomialTerm 1 (Map.singleton n e)

-- 多項式
type Polynomial = Map.Map String PolynomialTerm

-- ゼロ多項式 つまり 0
zeroPolynomial :: Polynomial
zeroPolynomial = Map.empty

-- 単位多項式 つまり 1
unitPolynomial :: Polynomial
unitPolynomial = Map.singleton "" (PolynomialTerm 1 Map.empty)

-- 仮定: 多項式は1変数であること
findTerm :: Polynomial -> Int -> Double
findTerm p degree = maybe 0 (\(PolynomialTerm c _) -> c) term
  where
    pairs = Map.toList p
    terms = map snd pairs
    term = List.find (\term -> degreeOfTerm term == degree) terms

-- 多項式の次数を返す
degreeOfPolynomial :: Polynomial -> Int
degreeOfPolynomial p =
  if Map.null p
    then 0
    else maximum (map degreeOfTerm (Map.elems p))

-- 項の次数を返す
degreeOfTerm :: PolynomialTerm -> Int
degreeOfTerm (PolynomialTerm _ var) = sum (Map.elems var)

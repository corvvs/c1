
module PolynomialBase(PolynomialVariable (..), PolynomialTerm (..), Polynomial (..), zeroTerm, polynomialTermByNum, polynomialTermByVar, zeroPolynomial, unitPolynomial, getTerm, getCoeffOfTerm, dimensionOfPolynomial, minDemensionOfPolynomial, degreeOfTerm) where

import AST (AST (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Debug.Trace (trace)
import TypeClass (Addable (..), Multipliable (..), Subtractable (..))

-- 変数
type PolynomialVariable = Map.Map String Int

-- 項
data PolynomialTerm = PolynomialTerm Double PolynomialVariable

zeroTerm :: PolynomialTerm
zeroTerm = PolynomialTerm 0 Map.empty

unitTerm :: PolynomialTerm
unitTerm = PolynomialTerm 1 Map.empty

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


getTerm :: Polynomial -> Int -> Maybe PolynomialTerm
getTerm p degree = List.find (\term -> degreeOfTerm term == degree) terms
  where
    terms = Map.elems p

-- 仮定: 多項式は1変数であること
getCoeffOfTerm :: Polynomial -> Int -> Double
getCoeffOfTerm p degree = maybe 0 (\(PolynomialTerm c _) -> c) term
  where
    term = getTerm p degree

-- 多項式の次数を返す
dimensionOfPolynomial :: Polynomial -> Int
dimensionOfPolynomial p =
  if Map.null p
    then 0
    else maximum (map degreeOfTerm (Map.elems p))

-- 多項式の最小次数を返す
minDemensionOfPolynomial :: Polynomial -> Int
minDemensionOfPolynomial p =
  if Map.null p
    then 0
    else minimum (map degreeOfTerm (Map.elems p))

-- 項の次数を返す
degreeOfTerm :: PolynomialTerm -> Int
degreeOfTerm (PolynomialTerm _ var) = sum (Map.elems var)

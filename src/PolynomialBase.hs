
module PolynomialBase(PolynomialVariable (..), PolynomialTerm (..), Polynomial (..), zeroTerm, polynomialTermByNum, polynomialTermByVar, zeroPolynomial, unitPolynomial, getTerm, getCoeffOfTerm, dimensionOfPolynomial, minDemensionOfPolynomial, dimensionOfTerm) where

import AST (AST (..))
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace (trace)
import TypeClass (Addable (..), Multipliable (..), Subtractable (..))
import qualified Data.Text as T

-- 変数
type PolynomialVariable = Map.Map T.Text Int

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
type Polynomial = Map.Map T.Text PolynomialTerm

-- ゼロ多項式 つまり 0
zeroPolynomial :: Polynomial
zeroPolynomial = Map.empty

-- 単位多項式 つまり 1
unitPolynomial :: Polynomial
unitPolynomial = Map.singleton T.empty (PolynomialTerm 1 Map.empty)


getTerm :: Polynomial -> Int -> Maybe PolynomialTerm
getTerm p degree = List.find (\term -> dimensionOfTerm term == degree) terms
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
    else Prelude.maximum (Prelude.map dimensionOfTerm (Map.elems p))

-- 多項式の最小次数を返す
minDemensionOfPolynomial :: Polynomial -> Int
minDemensionOfPolynomial p =
  if Map.null p
    then 0
    else Prelude.minimum (Prelude.map dimensionOfTerm (Map.elems p))

-- 項の次数を返す
dimensionOfTerm :: PolynomialTerm -> Int
dimensionOfTerm (PolynomialTerm _ var) = sum (Map.elems var)
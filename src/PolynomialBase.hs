{-# LANGUAGE InstanceSigs #-}

module PolynomialBase(PolynomialVariable, PolynomialTerm (..), Polynomial, zeroPolynomial, unitPolynomial, getTerm, getCoeffOfTerm, dimensionOfPolynomial, minDemensionOfPolynomial, dimensionOfTerm) where

import qualified Data.List as List
import qualified Data.Map as Map
import TypeClass (Addable (..), Multipliable (..), Subtractable (..), Divisible (..))
import qualified Data.Text as T

-- 変数
type PolynomialVariable = Map.Map T.Text Int

-- 項
data PolynomialTerm = PolynomialTerm Double PolynomialVariable

zeroTerm :: PolynomialTerm
zeroTerm = PolynomialTerm 0 Map.empty

-- unitTerm :: PolynomialTerm
-- unitTerm = PolynomialTerm 1 Map.empty

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

-- 変数の積
mulVar :: PolynomialVariable -> PolynomialVariable -> PolynomialVariable
mulVar = Map.unionWith (+)

-- 変数の商
divVar :: PolynomialVariable -> PolynomialVariable -> PolynomialVariable
divVar t1 t2 = Map.filter (/= 0) added
  where
    t2' = Map.map negate t2
    added = Map.unionWith (+) t1 t2'

-- 項同士の和
instance Addable PolynomialTerm where
  (<+>) :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  (<+>) (PolynomialTerm c1 v1) (PolynomialTerm c2 v2)
    | c1 == 0 = Just (PolynomialTerm c2 v2)
    | c2 == 0 = Just (PolynomialTerm c1 v1)
    | v1 == v2 = Just (PolynomialTerm (c1 + c2) v1)
    | otherwise = Nothing

-- 項同士の差
instance Subtractable PolynomialTerm where
  (<->) :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  (<->) (PolynomialTerm c1 v1) (PolynomialTerm c2 v2)
    | c1 == 0 = Just (PolynomialTerm (-c2) v2)
    | c2 == 0 = Just (PolynomialTerm c1 v1)
    | v1 == v2 = Just (PolynomialTerm (c1 - c2) v1)
    | otherwise = Nothing

-- 項同士の積
instance Multipliable PolynomialTerm where
  (<**>) :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  (<**>) (PolynomialTerm 0 _) _ = Just zeroTerm
  (<**>) _ (PolynomialTerm 0 _) = Just zeroTerm
  (<**>) (PolynomialTerm c1 v1) (PolynomialTerm c2 v2) = Just (PolynomialTerm (c1 * c2) (mulVar v1 v2))

-- 項同士の商
instance Divisible PolynomialTerm where
  (</>) :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  (</>) (PolynomialTerm 0 _) _ = Just zeroTerm
  (</>) _ (PolynomialTerm 0 _) = error "Zero Division"
  (</>) (PolynomialTerm c1 v1) (PolynomialTerm c2 v2) = Just (PolynomialTerm (c1 / c2) (divVar v1 v2))

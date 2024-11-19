module Polynomial (PolynomialTerm (..), PolynomialVariable, Polynomial, printPolynomial, polynomialSignature, transformToStandard, inspectPolynomialInfo, isSolvable, degreeOfTerm) where

import AST (AST (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace)
import MyPrint (showNumber)
import PolynomialBase
import TypeClass (Addable (..), Multipliable (..), Subtractable (..))

-- 変数の積
mulVar :: PolynomialVariable -> PolynomialVariable -> PolynomialVariable
mulVar = Map.unionWith (+)

polynomialVarSignature :: PolynomialVariable -> String
polynomialVarSignature var = joined
  where
    signatures = map (uncurry polynomialVarSignature') (filter (\(g, d) -> d /= 0) (Map.toList var))
    joined = case signatures of
      [] -> ""
      _ -> foldr1 (\a b -> a ++ " " ++ b) signatures

polynomialVarSignature' :: String -> Int -> String
polynomialVarSignature' g d = case d of
  0 -> ""
  1 -> g
  _ -> g ++ "^" ++ show d


polynomialTermCoefficientSignature :: PolynomialTerm -> String
polynomialTermCoefficientSignature (PolynomialTerm c _) = case c of
  c | c /= 0 -> showNumber c
  _ -> ""

polynomialTermSignature :: PolynomialTerm -> String
polynomialTermSignature (PolynomialTerm 0 var) = ""
polynomialTermSignature (PolynomialTerm _ var) = polynomialVarSignature var

polynomialTermPrint :: Int -> PolynomialTerm -> String
polynomialTermPrint index term = case term of
  PolynomialTerm 0 _ -> ""
  _ | degreeOfTerm term == 0 -> coefficient
  PolynomialTerm c _ | abs c == 1 -> coefficient ++ variable
  _ -> coefficient ++ "*" ++ variable
  where
    PolynomialTerm c var = term
    -- 前提: c /= 0
    sign
      | c > 0 =
          if index == 0
            then ""
            else "+ "
      | otherwise = "- "
    coefficient
      | not (Map.null var) && (abs c == 1) = sign
      | otherwise = sign ++ show (abs c)
    variable = polynomialVarSignature var

-- 項同士の和
instance Addable PolynomialTerm where
  add :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  add (PolynomialTerm c1 v1) (PolynomialTerm c2 v2)
    | c1 == 0 = Just (PolynomialTerm c2 v2)
    | c2 == 0 = Just (PolynomialTerm c1 v1)
    | v1 == v2 = Just (PolynomialTerm (c1 + c2) v1)
    | otherwise = Nothing

-- 項同士の差
instance Subtractable PolynomialTerm where
  sub :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  sub (PolynomialTerm c1 v1) (PolynomialTerm c2 v2)
    | c1 == 0 = Just (PolynomialTerm (-c2) v2)
    | c2 == 0 = Just (PolynomialTerm c1 v1)
    | v1 == v2 = Just (PolynomialTerm (c1 - c2) v1)
    | otherwise = Nothing

-- 項同士の積
instance Multipliable PolynomialTerm where
  mul :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  mul (PolynomialTerm 0 _) _ = Just (PolynomialTerm 0 Map.empty)
  mul _ (PolynomialTerm 0 _) = Just (PolynomialTerm 0 Map.empty)
  mul (PolynomialTerm c1 v1) (PolynomialTerm c2 v2) = Just (PolynomialTerm (c1 * c2) (mulVar v1 v2))

-- 多項式の符号を反転
flipPolynomialSign :: Polynomial -> Polynomial
flipPolynomialSign = Map.map (\(PolynomialTerm c v) -> PolynomialTerm (-c) v)

-- ゼロの項を除去する
reducePolynomial :: Polynomial -> Polynomial
reducePolynomial = Map.filter (\(PolynomialTerm c _) -> c /= 0)

-- 1つの項を多項式に掛けた結果を返す
mulTermPolynomial :: PolynomialTerm -> Polynomial -> Polynomial
mulTermPolynomial s p = reduced
  where
    pairs = Map.toList p
    muled = polynomialByTerms (map (\(k, t) -> fromJust (mul s t)) pairs)
    reduced = reducePolynomial muled

instance Addable Polynomial where
  add :: Polynomial -> Polynomial -> Maybe Polynomial
  add p1 p2 = Just (reducePolynomial united)
    where
      f :: PolynomialTerm -> PolynomialTerm -> PolynomialTerm
      f t1 t2 = fromJust (add t1 t2)
      united = Map.unionWith f p1 p2

instance Subtractable Polynomial where
  sub :: Polynomial -> Polynomial -> Maybe Polynomial
  sub p1 p2 =
    let untied =
          Map.unionWith
            (\t1 t2 -> fromJust (add t1 t2))
            p1
            (flipPolynomialSign p2)
     in Just (reducePolynomial untied)

instance Multipliable Polynomial where
  mul :: Polynomial -> Polynomial -> Maybe Polynomial
  mul p1 p2 = just
    where
      ts1 = Map.toList p1
      f :: (String, PolynomialTerm) -> Polynomial -> Polynomial
      f (sig, term) ac = fromJust added
        where
          pp = mulTermPolynomial term p2
          added = add ac pp
      folded = foldr f zeroPolynomial ts1
      just = Just folded

polynomialByTerms :: [PolynomialTerm] -> Polynomial
polynomialByTerms ts = Map.fromList (map (\t -> (polynomialTermSignature t, t)) ts)

polynomialByNum :: AST -> Polynomial
polynomialByNum (Num 0) = zeroPolynomial
polynomialByNum (Num a) = Map.singleton "" (PolynomialTerm a Map.empty)

polynomialByVar :: AST -> Polynomial
polynomialByVar (Var n e) = Map.singleton (polynomialVarSignature' n e) (PolynomialTerm 1 (Map.singleton n e))

polynomialSignature :: Polynomial -> String
polynomialSignature p = foldr (\(k, v) acc -> polynomialTermSignature v ++ " " ++ acc) "" (Map.toList p)

printPolynomial :: Polynomial -> String
printPolynomial p = case p of
  p | Map.null p -> "0"
  _ -> unwords indexedTerms
    where
      indexedTerms = zipWith polynomialTermPrint [0 ..] (Map.elems p)

transformToStandard :: AST -> Polynomial
transformToStandard (Num a) = polynomialByNum (Num a)
transformToStandard (Var n e) = polynomialByVar (Var n e)
transformToStandard
  (Add a b) = fromJust added
    where
      sa = transformToStandard a
      sb = transformToStandard b
      added = add sa sb
transformToStandard
  (Sub a b) = fromJust r
    where
      r = sub (transformToStandard a) (transformToStandard b)
transformToStandard
  (Mul a b) = fromJust r
    where
      r = mul (transformToStandard a) (transformToStandard b)
transformToStandard
  (Pow a b) = r
    where
      isConstant :: Polynomial -> Bool
      isConstant p = degreeOfPolynomial p == 0

      isNonNegativeInteger :: Double -> Bool
      isNonNegativeInteger n = n >= 0 && n == fromIntegral (round n)

      powPolynomial :: Polynomial -> Double -> Polynomial
      powPolynomial p n = if isNonNegativeInteger n
          then powPolynomial' unitPolynomial p (round n)
          else error "Not supported"

      powPolynomial' :: Polynomial -> Polynomial -> Int -> Polynomial
      powPolynomial' p q n
        | n <= 0 = p
        | otherwise = powPolynomial' (fromJust (mul p q)) q (n - 1)

      sa = transformToStandard a
      sb = transformToStandard b
      a0 = findTerm sa 0
      b0 = findTerm sb 0
      r = case (isConstant sa, isConstant sb) of
        (_, False) -> error "Not supported1"
        (True, _) -> polynomialByNum (Num (a0 ** b0))
        (False, _) -> powPolynomial sa b0
    

type PolynomialInfo = (Set.Set String, Int)

inspectPolynomialInfo :: Polynomial -> PolynomialInfo
inspectPolynomialInfo p = (varSet, degree)
  where
    varSet = polynomialVarSet p
    degree = degreeOfPolynomial p

-- 多項式がサポート範囲内かどうかを判定する. つまり:
-- - 文字1種類以下
-- - 次数2以下
isSolvable :: PolynomialInfo -> (Bool, String)
isSolvable (s, d)
  | Set.size s > 1 = (False, "Too many variables")
  | d > 2 = (False, "Too large dimension")
  | otherwise = (True, "ok: This is a supported equation")

-- 多項式の変数集合を返す
-- -> すべての項の変数集合の和集合
polynomialVarSet :: Polynomial -> Set.Set String
polynomialVarSet p = Set.unions (map (\(k, t) -> termVarSet t) (Map.toList p))
  where
    -- 項の変数集合を返す
    termVarSet :: PolynomialTerm -> Set.Set String
    termVarSet (PolynomialTerm _ var) = Map.keysSet var


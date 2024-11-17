module Polynomial (PolynomialTerm (..), PolynomialVariable, Polynomial, polynomialSignature, transformoToStandard) where

import AST (AST (..))
import Data.ByteString qualified as List
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import TypeClass (Addable (..), Multipliable (..), Subtractable (..))

-- 変数
type PolynomialVariable = Map.Map String Int

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
polynomialVarSignature' g 0 = ""
polynomialVarSignature' g d = g ++ "^" ++ show d

-- 項
data PolynomialTerm = PolynomialTerm Double PolynomialVariable

-- 項のコンストラクタ by Num
polynomialTermByNum :: AST -> PolynomialTerm
polynomialTermByNum (Num a) = PolynomialTerm a Map.empty

-- 項のコンストラクタ by Var
polynomialTermByVar :: AST -> PolynomialTerm
polynomialTermByVar (Var n e) = PolynomialTerm 1 (Map.singleton n e)

polynomialTermCoefficientSignature :: PolynomialTerm -> String
polynomialTermCoefficientSignature (PolynomialTerm c _) = case c of
  0 -> ""
  c | c > 0 -> "+ " ++ show (abs c)
  c | c < 0 -> "- " ++ show (abs c)

polynomialTermSignature :: PolynomialTerm -> String
polynomialTermSignature (PolynomialTerm 0 var) = ""
polynomialTermSignature (PolynomialTerm _ var) = trace ("[polynomialTermSignature] " ++ show var ++ "->[" ++ s ++ "]") s
  where
    s = polynomialVarSignature var

polynomialTermPrint :: PolynomialTerm -> String
polynomialTermPrint (PolynomialTerm c var) = case c of
  c | c > 0 -> "+ " ++ show (abs c) ++ polynomialVarSignature var
  c | c < 0 -> "- " ++ show (abs c) ++ polynomialVarSignature var
  _ -> ""

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

-- 多項式
type Polynomial = Map.Map String PolynomialTerm

-- ゼロ多項式 つまり 0
zeroPolynomial :: Polynomial
zeroPolynomial = Map.empty

-- 単位多項式 つまり 1
unitPolynomial :: Polynomial
unitPolynomial = Map.singleton "" (PolynomialTerm 1 Map.empty)

-- 多項式の符号を反転
flipPolynomialSign :: Polynomial -> Polynomial
flipPolynomialSign = Map.map (\(PolynomialTerm c v) -> PolynomialTerm (-c) v)

-- ゼロの項を除去する
reducePolynomial :: Polynomial -> Polynomial
reducePolynomial = Map.filter (\(PolynomialTerm c _) -> c /= 0)

-- 1つの項を多項式に掛けた結果を返す
mulTermPolynomial :: PolynomialTerm -> Polynomial -> Polynomial
mulTermPolynomial s p = trace ("[mulTermPolynomial reduced] " ++ polynomialSignature reduced) reduced
  where
    pairs = Map.toList p
    muled = polynomialByTerms (map (\(k, t) -> trace ("[mulTermPolynomial] [" ++ k ++ "]") fromJust (mul s t)) pairs)
    reduced = reducePolynomial (trace ("[mulTermPolynomial muled] " ++ polynomialSignature muled) muled)

instance Addable Polynomial where
  add :: Polynomial -> Polynomial -> Maybe Polynomial
  add p1 p2 = Just (reducePolynomial united)
    where
      f :: PolynomialTerm -> PolynomialTerm -> PolynomialTerm
      f t1 t2 = trace ("[poly add unionWith] " ++ polynomialTermPrint a) a
        where
          a = fromJust (add t1 t2)
      united =
        Map.unionWith f (trace ("[poly add p1]" ++ polynomialSignature p1) p1) (trace ("[poly add p2]" ++ polynomialSignature p2) p2)

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
      ts1 = Map.toList (trace ("mul p1: " ++ polynomialSignature p1 ++ " p2: " ++ polynomialSignature p2) p1)
      f :: (String, PolynomialTerm) -> Polynomial -> Polynomial
      f (sig, term) ac = fromJust added
        where
          pp = mulTermPolynomial term p2
          added =
            add
              ( trace
                  ("mul ac: " ++ polynomialSignature ac)
                  ac
              )
              (trace ("mul pp: " ++ polynomialSignature pp) pp)
      folded = foldr f zeroPolynomial ts1
      just = Just folded

polynomialByTerms :: [PolynomialTerm] -> Polynomial
polynomialByTerms ts = Map.fromList (map (\t -> (polynomialTermSignature t, t)) ts)

polynomialByNum :: AST -> Polynomial
polynomialByNum (Num a) = Map.singleton "" (PolynomialTerm a Map.empty)

polynomialByVar :: AST -> Polynomial
polynomialByVar (Var n e) = Map.singleton (polynomialVarSignature' n e) (PolynomialTerm 1 (Map.singleton n e))

polynomialSignature :: Polynomial -> String
-- polynomialSignature p = foldr (\(k, v) acc -> "key[" ++ k ++ "]:val[" ++ polynomialTermPrint v ++ "] " ++ acc) "" (Map.toList p)
polynomialSignature p = foldr (\(k, v) acc -> polynomialTermPrint v ++ " " ++ acc) "" (Map.toList p)

transformoToStandard :: AST -> Polynomial
transformoToStandard (Num a) = polynomialByNum (Num a)
transformoToStandard (Var n e) = polynomialByVar (Var n e)
transformoToStandard
  (Add a b) = fromJust added
    where
      sa = transformoToStandard a
      sb = transformoToStandard b
      added = add (trace ("[transformoToStandard Add sa] " ++ polynomialSignature sa) sa) (trace ("[transformoToStandard Add sb] " ++ polynomialSignature sb) sb)
transformoToStandard
  (Sub a b) = fromJust r
    where
      r = sub (transformoToStandard a) (transformoToStandard b)
transformoToStandard
  (Mul a b) = fromJust r
    where
      r = mul (transformoToStandard a) (transformoToStandard b)
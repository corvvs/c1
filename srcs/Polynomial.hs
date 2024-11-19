module Polynomial (PolynomialTerm (..), PolynomialVariable, Polynomial, PolynomialInfo(..), printPolynomial, polynomialSignature, transformToStandard, inspectPolynomialInfo, isSolvable, degreeOfTerm) where

import AST (AST (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Debug.Trace (trace)
import MyPrint (showNumber)
import PolynomialBase
import TypeClass (Addable (..), Multipliable (..), Subtractable (..), Divisible (..))

-- 変数の積
mulVar :: PolynomialVariable -> PolynomialVariable -> PolynomialVariable
mulVar = Map.unionWith (+)

-- 変数の商
divVar :: PolynomialVariable -> PolynomialVariable -> PolynomialVariable
divVar t1 t2 = Map.filter (/= 0) added
  where
    t2' = Map.map negate t2
    added = Map.unionWith (+) t1 t2'

polynomialVarSignature :: PolynomialVariable -> T.Text
polynomialVarSignature var = joined
  where
    signatures = map (uncurry polynomialVarSignature') (filter (\(g, d) -> d /= 0) (Map.toList var))
    joined = case signatures of
      [] -> T.empty
      _ -> foldr1 (\a b -> T.concat [a, T.pack " ", b]) signatures

polynomialVarSignature' :: T.Text -> Int -> T.Text
polynomialVarSignature' g d = case d of
  0 -> T.empty
  1 -> g
  _ -> T.concat [g, T.pack "^", T.pack $ show d]


polynomialTermCoefficientSignature :: PolynomialTerm -> T.Text
polynomialTermCoefficientSignature (PolynomialTerm c _) = case c of
  c | c /= 0 -> showNumber c
  _ -> T.empty

polynomialTermSignature :: PolynomialTerm -> T.Text
polynomialTermSignature (PolynomialTerm 0 var) = T.empty
polynomialTermSignature (PolynomialTerm _ var) = polynomialVarSignature var

polynomialTermPrint :: Int -> PolynomialTerm -> T.Text
polynomialTermPrint index term = case term of
  PolynomialTerm 0 _ -> T.empty
  _ | degreeOfTerm term == 0 -> coefficient
  PolynomialTerm c _ | abs c == 1 -> T.concat [coefficient, variable]
  _ -> T.concat [coefficient, T.pack "*", variable]
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
      | not (Map.null var) && (abs c == 1) = T.pack sign
      | otherwise = T.pack (sign ++ show (abs c))
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
  mul (PolynomialTerm 0 _) _ = Just zeroTerm
  mul _ (PolynomialTerm 0 _) = Just zeroTerm
  mul (PolynomialTerm c1 v1) (PolynomialTerm c2 v2) = Just (PolynomialTerm (c1 * c2) (mulVar v1 v2))

-- 項同士の商
instance Divisible PolynomialTerm where
  div :: PolynomialTerm -> PolynomialTerm -> Maybe PolynomialTerm
  div (PolynomialTerm 0 _) _ = Just zeroTerm
  div _ (PolynomialTerm 0 _) = error "Zero Division"
  div (PolynomialTerm c1 v1) (PolynomialTerm c2 v2) = Just (PolynomialTerm (c1 / c2) (divVar v1 v2))

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

divTermPolynomial :: Polynomial -> PolynomialTerm -> Polynomial
divTermPolynomial p t = reduced
  where
    pairs = Map.toList p
    divided = polynomialByTerms (map (\(k, t') -> fromJust (TypeClass.div t' t)) pairs)
    reduced = reducePolynomial divided

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
      f :: (T.Text, PolynomialTerm) -> Polynomial -> Polynomial
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
polynomialByNum (Num a) = Map.singleton T.empty (PolynomialTerm a Map.empty)

polynomialByVar :: AST -> Polynomial
polynomialByVar (Var n e) = Map.singleton (polynomialVarSignature' n e) (PolynomialTerm 1 (Map.singleton n e))

polynomialSignature :: Polynomial -> T.Text
polynomialSignature p = foldr (\(k, v) acc -> T.concat [polynomialTermSignature v, T.pack " ", acc]) T.empty (Map.toList p)

printPolynomial :: Polynomial -> T.Text
printPolynomial p = case p of
  p | Map.null p -> T.pack "0"
  _ -> T.unwords indexedTerms
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
-- 除算: いろいろ頑張る
transformToStandard
  (Div a b) = r
    where
      isConstant :: Polynomial -> Bool
      isConstant p = dimensionOfPolynomial p == 0

      isZero :: Polynomial -> Bool
      isZero p = isConstant p && getCoeffOfTerm p 0 == 0

      isMonomial :: Polynomial -> Bool
      isMonomial p = dimensionOfPolynomial p == minDemensionOfPolynomial p

      divideByConstant :: Polynomial -> Double -> Polynomial
      divideByConstant p n = Map.map (\(PolynomialTerm c v) -> PolynomialTerm (c / n) v) p

      sa = transformToStandard a
      sb = transformToStandard b

      r = case sb of
        sb | isZero sb -> error "Division by zero"
        sb | isConstant sb -> divideByConstant sa (getCoeffOfTerm sb 0)
        sb | isMonomial sb -> let d = dimensionOfPolynomial sb in divTermPolynomial sa (fromJust (getTerm sb d))
        _ -> error "Division by zero"

-- 冪乗: いろいろ頑張る
transformToStandard
  (Pow a b) = r
    where
      isConstant :: Polynomial -> Bool
      isConstant p = dimensionOfPolynomial p == 0

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
      a0 = getCoeffOfTerm sa 0
      b0 = getCoeffOfTerm sb 0
      r = case (isConstant sa, isConstant sb) of
        (_, False) -> error "Not supported1"
        (True, _) -> polynomialByNum (Num (a0 ** b0))
        (False, _) -> powPolynomial sa b0
    

data PolynomialInfo = PolynomialInfo {
  varSet :: Set.Set T.Text,
  maxDimension :: Int,
  minDimension :: Int
}
  deriving (Show)

inspectPolynomialInfo :: Polynomial -> PolynomialInfo
inspectPolynomialInfo p = PolynomialInfo {
    varSet = polynomialVarSet p,
    maxDimension = dimensionOfPolynomial p,
    minDimension = minDemensionOfPolynomial p
  }

-- 多項式がサポート範囲内かどうかを判定する. つまり:
-- - 文字1種類以下
-- - 次数2以下
isSolvable :: PolynomialInfo -> (Bool, T.Text)
isSolvable p = case (s, maxD, minD) of
    _ | Set.size s > 1 -> (False, T.pack "Too many variables")
    _ | maxD > 2 -> (False, T.pack "Too large dimension")
    _ | minD < 0 -> (False, T.pack "Fractional dimension")
    _ | otherwise -> (True, T.pack "ok: This is a supported equation")
  where
      s = varSet p
      maxD = maxDimension p
      minD = minDimension p


-- 多項式の変数集合を返す
-- -> すべての項の変数集合の和集合
polynomialVarSet :: Polynomial -> Set.Set T.Text
polynomialVarSet p = Set.unions (map (\(k, t) -> termVarSet t) (Map.toList p))
  where
    -- 項の変数集合を返す
    termVarSet :: PolynomialTerm -> Set.Set T.Text
    termVarSet (PolynomialTerm _ var) = Map.keysSet var


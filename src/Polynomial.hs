module Polynomial (PolynomialInfo(..), printPolynomial, polynomialSignature, reduceToPolynomialI, inspectPolynomialInfo, isSolvable) where

import AST (AST (..))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Exception
import qualified Data.Text as T
import TypeClass (Addable (..), Multipliable (..), Divisible (..))
import PolynomialBase

polynomialVarSignature :: PolynomialVariable -> T.Text
polynomialVarSignature var = joined
  where
    signatures = map (uncurry polynomialVarSignature') (filter (\(_, d) -> d /= 0) (Map.toList var))
    joined = case signatures of
      [] -> T.empty
      _ -> foldr1 (\a b -> T.concat [a, T.pack " ", b]) signatures

polynomialVarSignature' :: T.Text -> Int -> T.Text
polynomialVarSignature' g d = case d of
  0 -> T.empty
  1 -> g
  _ -> T.concat [g, T.pack "^", T.pack $ show d]

polynomialTermSignature :: PolynomialTerm -> T.Text
polynomialTermSignature (PolynomialTerm 0 _) = T.empty
polynomialTermSignature (PolynomialTerm _ var) = polynomialVarSignature var

polynomialTermPrint :: Int -> PolynomialTerm -> T.Text
polynomialTermPrint index term = case term of
  PolynomialTerm 0 _ -> T.empty
  _ | dimensionOfTerm term == 0 -> coefficient
  PolynomialTerm c' _ | abs c' == 1 -> T.concat [coefficient, variable]
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
    muled = polynomialByTerms (map (\(_, t) -> fromJust (TypeClass.mul s t)) pairs)
    reduced = reducePolynomial muled

divTermPolynomial :: Polynomial -> PolynomialTerm -> Polynomial
divTermPolynomial p t = reduced
  where
    pairs = Map.toList p
    divided = polynomialByTerms (map (\(_, t') -> fromJust (TypeClass.div t' t)) pairs)
    reduced = reducePolynomial divided

addPolynomials :: Polynomial -> Polynomial -> Maybe Polynomial
addPolynomials p1 p2 = Just (reducePolynomial united)
  where
    f :: PolynomialTerm -> PolynomialTerm -> PolynomialTerm
    f t1 t2 = fromJust (add t1 t2)
    united = Map.unionWith f p1 p2

subPolynomials :: Polynomial -> Polynomial -> Maybe Polynomial
subPolynomials p1 p2 =
    let untied =
          Map.unionWith
            (\t1 t2 -> fromJust (add t1 t2))
            p1
            (flipPolynomialSign p2)
     in Just (reducePolynomial untied)

mulPolynomials :: Polynomial -> Polynomial -> Maybe Polynomial
mulPolynomials p1 p2 = just
    where
      ts1 = Map.toList p1
      f :: (T.Text, PolynomialTerm) -> Polynomial -> Polynomial
      f (_, term) ac = fromJust added
        where
          pp = mulTermPolynomial term p2
          added = addPolynomials ac pp
      folded = foldr f zeroPolynomial ts1
      just = Just folded

polynomialByTerms :: [PolynomialTerm] -> Polynomial
polynomialByTerms ts = Map.fromList (map (\t -> (polynomialTermSignature t, t)) ts)

polynomialByNum :: AST -> Polynomial
polynomialByNum (Num 0) = zeroPolynomial
polynomialByNum (Num a) = Map.singleton T.empty (PolynomialTerm a Map.empty)
polynomialByNum _ = error "Not a number"

polynomialByVar :: AST -> Polynomial
polynomialByVar (Var n e) = Map.singleton (polynomialVarSignature' n e) (PolynomialTerm 1 (Map.singleton n e))
polynomialByVar _ = error "Not a variable"

polynomialSignature :: Polynomial -> T.Text
polynomialSignature p = foldr (\(_, v) acc -> T.concat [polynomialTermSignature v, T.pack " ", acc]) T.empty (Map.toList p)

printPolynomial :: Polynomial -> T.Text
printPolynomial p = case p of
  p' | Map.null p' -> T.pack "0"
  _ -> T.unwords indexedTerms
    where
      terms = Map.elems p
      sortedTerms = List.sortBy (\t1 t2 -> compare (dimensionOfTerm t1) (dimensionOfTerm t2)) terms
      indexedTerms = zipWith polynomialTermPrint [0 ..] sortedTerms

reduceToPolynomialI :: AST -> ExceptTT Polynomial
reduceToPolynomialI a = return (reduceToPolynomial a)

reduceToPolynomial :: AST -> Polynomial
reduceToPolynomial (Num a) = polynomialByNum (Num a)
reduceToPolynomial (Var n e) = polynomialByVar (Var n e)
reduceToPolynomial (Add a b) = fromJust added
  where
    sa = reduceToPolynomial a
    sb = reduceToPolynomial b
    added = addPolynomials sa sb
reduceToPolynomial (Sub a b) = fromJust r
  where
    r = subPolynomials (reduceToPolynomial a) (reduceToPolynomial b)
reduceToPolynomial (Mul a b) = fromJust r
  where
    r = mulPolynomials (reduceToPolynomial a) (reduceToPolynomial b)
-- 除算: いろいろ頑張る
reduceToPolynomial (Div a b) = r
  where
    isConstant :: Polynomial -> Bool
    isConstant p = dimensionOfPolynomial p == 0

    isZero :: Polynomial -> Bool
    isZero p = isConstant p && getCoeffOfTerm p 0 == 0

    isMonomial :: Polynomial -> Bool
    isMonomial p = dimensionOfPolynomial p == minDemensionOfPolynomial p

    divideByConstant :: Polynomial -> Double -> Polynomial
    divideByConstant p n = Map.map (\(PolynomialTerm c v) -> PolynomialTerm (c / n) v) p

    sa = reduceToPolynomial a
    sb = reduceToPolynomial b

    r = case sb of
      sb' | isZero sb' -> error "Division by zero"
      sb' | isConstant sb' -> divideByConstant sa (getCoeffOfTerm sb' 0)
      sb' | isMonomial sb' -> let d = dimensionOfPolynomial sb' in divTermPolynomial sa (fromJust (getTerm sb' d))
      _ -> error "Division by Polynomial is not supported"

-- 冪乗: いろいろ頑張る
reduceToPolynomial (Pow a b) = r
    where
      isConstant :: Polynomial -> Bool
      isConstant p = dimensionOfPolynomial p == 0

      isNonNegativeInteger :: Double -> Bool
      isNonNegativeInteger n = n >= 0 && n == fromIntegral (round n :: Integer)

      powPolynomial :: Polynomial -> Double -> Polynomial
      powPolynomial p n = if isNonNegativeInteger n
          then powPolynomial' unitPolynomial p (round n)
          else error "Not supported"

      powPolynomial' :: Polynomial -> Polynomial -> Int -> Polynomial
      powPolynomial' p q n
        | n <= 0 = p
        | otherwise = powPolynomial' (fromJust (mulPolynomials p q)) q (n - 1)

      sa = reduceToPolynomial a
      sb = reduceToPolynomial b
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

inspectPolynomialInfo :: Polynomial -> ExceptTT PolynomialInfo
inspectPolynomialInfo p = return PolynomialInfo {
    varSet = polynomialVarSet p,
    maxDimension = dimensionOfPolynomial p,
    minDimension = minDemensionOfPolynomial p
  }

-- 多項式がサポート範囲内かどうかを判定する. つまり:
-- - 文字1種類以下
-- - 次数2以下
isSolvable :: PolynomialInfo -> ExceptTT (Bool, T.Text)
isSolvable p = return $ case (s, maxD, minD) of
    _ | Set.size s > 1 -> (False, T.pack "Too many variables")
    _ | maxD > 3 -> (False, T.pack "Too large dimension")
    _ | minD < 0 -> (False, T.pack "Fractional dimension")
    _ | otherwise -> (True, T.pack "ok: This is a supported equation")
  where
      s = varSet p
      maxD = maxDimension p
      minD = minDimension p


-- 多項式の変数集合を返す
-- -> すべての項の変数集合の和集合
polynomialVarSet :: Polynomial -> Set.Set T.Text
polynomialVarSet p = Set.unions (map (\(_, t) -> termVarSet t) (Map.toList p))
  where
    -- 項の変数集合を返す
    termVarSet :: PolynomialTerm -> Set.Set T.Text
    termVarSet (PolynomialTerm _ var) = Map.keysSet var


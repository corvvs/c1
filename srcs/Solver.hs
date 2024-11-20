module Solver (solveEquation) where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import MyPrint
import MyPrint qualified
import PolynomialBase
import Polynomial
import Data.Complex qualified as C

solveEquation :: Polynomial -> IO ()
solveEquation p = do
  let a3 = getCoeffOfTerm p 3
  let a2 = getCoeffOfTerm p 2
  let a1 = getCoeffOfTerm p 1
  let a0 = getCoeffOfTerm p 0
  -- putStrLn ("(a, b, c) = (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")")
  case (a3, a2, a1, a0) of
    (0, 0, 0, _) -> solveEquation0 a0
    (0, 0, _, _) -> solveEquation1 a1 a0
    (0, _, _, _) -> solveEquation2 a2 a1 a0
    (_, _, _, _) -> solveEquation3 a3 a2 a1 a0

-- "0次方程式" c = 0 を解く
solveEquation0 :: Double -> IO ()
solveEquation0 0 = MyPrint.printLine "Solutions" $ T.pack "ARBITRARY COMPLEX NUMBER"
solveEquation0 _ = MyPrint.printLine "Solutions" $ T.pack "NONE"

-- 1次方程式 bx + c = 0 を解く -> x = -c / b
solveEquation1 :: Double -> Double -> IO ()
solveEquation1 b c = do
  let x = -(c / b)
  MyPrint.printLine "Solutions" $ MyPrint.showNumber x

-- 2次方程式 ax^2 + bx + c = 0 を解く
solveEquation2 :: Double -> Double -> Double -> IO ()
solveEquation2 a b c = do
  let d = discriminant2 a b c
  let polarity = case d of
        d | d > 0 -> "+"
        d | d == 0 -> "0"
        d | d < 0 -> "-"
  MyPrint.printLine "Discriminant" $ T.pack $ show d ++ "(" ++ polarity ++ ")"
  let solutions =
        ( case d of
            d | d > 0 -> solveEquation2Positive a b c
            d | d < 0 -> solveEquation2Negative a b c
            _ -> solveEquation2Zero a b c
        )
  MyPrint.printLine "Solutions" solutions

-- 2次方程式の判別式 D = b^2 - 4ac
discriminant2 :: Double -> Double -> Double -> Double
discriminant2 a b c = b ^ 2 - 4 * a * c

-- 2次方程式の解(D > 0)
solveEquation2Positive :: Double -> Double -> Double -> T.Text
solveEquation2Positive a b c = T.concat[MyPrint.showNumber x1, T.pack ", ", MyPrint.showNumber x2]
  where
    d = discriminant2 a b c
    x1 = (-b + sqrt d) / (2 * a)
    x2 = (-b - sqrt d) / (2 * a)

-- 2次方程式の解(D = 0)
solveEquation2Zero :: Double -> Double -> Double -> T.Text
solveEquation2Zero a b c =
  let x = -(b / (2 * a)) in MyPrint.showNumber x

-- 2次方程式の解(D < 0)
solveEquation2Negative :: Double -> Double -> Double -> T.Text
solveEquation2Negative a b c = T.pack $ pr ++ pi
  where
    d = discriminant2 a b c
    denominator = 2 * a
    real = -(b / denominator)
    pr =
      if real == 0
        then ""
        else show real ++ " "
    imaginary = sqrt (-d) / denominator
    pi =
      if imaginary >= 0
        then "+/- " ++ show imaginary ++ " i"
        else "-/+ " ++ show (abs imaginary) ++ " i"

type MC = C.Complex Double

-- 3次方程式 a_3x^3 + a_2x^2 + a_1x + a_0 = 0 を解く
solveEquation3 :: Double -> Double -> Double -> Double -> IO ()
solveEquation3 a_3 a_2 a_1 a_0 = do
  let d = discriminant3 a_3 a_2 a_1 a_0
  let polarity = case d of
        d | d > 0 -> "+"
        d | d == 0 -> "0"
        d | d < 0 -> "-"
  MyPrint.printLine "Discriminant" $ T.pack $ show d ++ "(" ++ polarity ++ ")"
  let solutions = step1 a_3 a_2 a_1 a_0
  MyPrint.printLine "Solutions" $ T.pack $ foldr1 (\a b -> a ++ ", " ++ b) $ map showComplex solutions

  where
    showComplex :: MC -> String
    showComplex (r C.:+ i) = showComplex' r i

    showComplex' :: Double -> Double -> String
    showComplex' real imaginary = case (real, imaginary) of
      (0, 0) -> show $ abs real
      (0, _) -> pi
      (_, 0) -> pr
      (_, _) -> pr ++ pi
      where
        pr =
          if real == 0
            then ""
            else show real ++ " "
        pi =
          if imaginary >= 0
            then "+ " ++ show imaginary ++ " i"
            else "- " ++ show (abs imaginary) ++ " i"

    discriminant3 :: Double -> Double -> Double -> Double -> Double
    discriminant3 a_3 a_2 a_1 a_0 = (-4) * a_1 ^ 3 * a_3 + a_1 ^ 2 * a_2 ^ 2 - 4 * a_0 * a_2 ^ 3 - 18 * a_0 * a_1 * a_2 * a_3 - 27 * a_0 ^ 2 * a_3 ^ 2

    step1 :: Double -> Double -> Double -> Double -> [MC]
    step1 a_3 a_2 a_1 a_0 = step2 aa_2 aa_1 aa_0
      where
        aa_0 = a_0 / a_3
        aa_1 = a_1 / a_3
        aa_2 = a_2 / a_3
    step2 :: Double -> Double -> Double -> [MC]
    step2 aa_2 aa_1 aa_0 = map (\y -> y - (aa_2 / 3 C.:+ 0.0)) ys
      where
        p = aa_1 - aa_2 ^ 2 / 3
        q = aa_0 - aa_1 * aa_2 / 3 + 2 * aa_2 ^ 3 / 27
        ys = step4 p q
    step4 :: Double -> Double -> [MC]
    step4 p q = [v + w, omega * v + omega ^ 2 * w, omega ^ 2 * v + omega * w]
      where
        omega = (-1) / 2 C.:+ sqrt 3 / 2
        s = (-q) / 2 C.:+ 0
        t = p / 3 C.:+ 0
        u = sqrt (s ^ 2 + t ^ 3)
        v =  (s + u) ** (1 / 3)
        w =  (s - u) ** (1 / 3)
    
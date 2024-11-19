module Solver (solveEquation) where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import MyPrint
import MyPrint qualified
import PolynomialBase
import Polynomial

solveEquation :: Polynomial -> IO ()
solveEquation p = do
  let a = getCoeffOfTerm p 2
  let b = getCoeffOfTerm p 1
  let c = getCoeffOfTerm p 0
  -- putStrLn ("(a, b, c) = (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")")
  case (a, b, c) of
    (0, 0, _) -> solveEquation0 c
    (0, _, _) -> solveEquation1 b c
    (_, _, _) -> solveEquation2 a b c

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

module Algebra (reduceEquation) where

import AST (AST(..))
import Parser (Equation (..))

-- 与えられた方程式を, 右辺が 0 になるように変形する
reduceEquation :: Equation -> Equation
reduceEquation (Equation (Num 0) rhs) = Equation rhs (Num 0)
reduceEquation (Equation lhs rhs) = case rhs of
  Num 0 -> Equation lhs rhs
  _ -> Equation (Sub lhs rhs) (Num 0)

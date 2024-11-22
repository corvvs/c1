module Algebra (reduceEquation) where

import Exception
import AST (AST(..))
import Parser (Equation (..))

-- 与えられた方程式を, 右辺が 0 になるように変形する
reduceEquation :: Equation ->  ExceptTT Equation
reduceEquation (Equation lhs rhs) = case rhs of
  Num 0 -> return $ Equation lhs rhs
  _ -> return $ Equation (Sub lhs rhs) (Num 0)

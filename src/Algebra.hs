module Algebra (reduceEquation) where

import Control.Monad.Except
import qualified Data.Text as T
import AST (AST(..))
import Parser (Equation (..))

-- 与えられた方程式を, 右辺が 0 になるように変形する
reduceEquation :: Equation ->  ExceptT T.Text IO Equation
reduceEquation (Equation lhs rhs) = case rhs of
  Num 0 -> return $ Equation lhs rhs
  _ -> return $ Equation (Sub lhs rhs) (Num 0)

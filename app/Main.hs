module Main (main) where

import Algebra
import Lexer
import MyPrint
import Parser (Equation (Equation), parseEquation)
import Polynomial
import Solver
import System.Environment
import qualified Data.Text as T

showUsage :: IO ()
showUsage = do
  path <- getProgName
  putStrLn (Prelude.unwords ["Usage:", path, "<expression>"])

printEquation :: Equation -> IO ()
printEquation (Equation lhs rhs) = do
  let pl = reduceToPolynomial lhs
  let pr = reduceToPolynomial rhs
  MyPrint.printLine "Raw form" $ T.concat [printPolynomial pl, T.pack " = ", printPolynomial pr]

solve :: T.Text -> IO ()
solve expression = do
  let tokens = lexer expression
  MyPrint.printLine "Tokens" $ T.pack $ show tokens

  let equation = parseEquation expression tokens
  MyPrint.printLine "AST" $ T.pack $ show equation
  printEquation equation

  let (Equation lhsAst _) = reduceEquation equation
  let polynomial = reduceToPolynomial lhsAst
  MyPrint.printLine "Reduced form" $ T.concat [printPolynomial polynomial, T.pack " = 0"]

  let pInfo = inspectPolynomialInfo polynomial
  let maxD = maxDimension pInfo
  MyPrint.printLine "Dimension" $ T.pack $ show maxD

  let (solvable, reason) = isSolvable pInfo
  if solvable
    then do
      solveEquation polynomial
    else putStrLn ("This equation is not solvable. (" ++ T.unpack reason ++ ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve (T.pack expression)
    _ -> showUsage

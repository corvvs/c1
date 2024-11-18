import Algebra
import Data.List qualified as List
import Data.Set qualified as Set
import Debug.Trace (trace)
import Lexer
import Parser ( parseEquation, Equation(Equation) )
import Polynomial
import Solver
import MyPrint
import System.Environment
import System.IO

showUsage :: IO ()
showUsage = do
  path <- getProgName
  putStrLn (unwords ["Usage:", path, "<expression>"])

solve :: String -> IO ()
solve expression = do
  let tokens = lexer expression
  MyPrint.printLine "Tokens" $ show tokens
  let equation = parseEquation tokens
  MyPrint.printLine  "AST" $ show equation
  let (Equation lhsAst rhsAst) = reduceEquation equation
  let polynomial = transformToStandard lhsAst
  -- 方程式標準形の表示
  MyPrint.printLine  "Reduced form" $ printPolynomial polynomial ++ " = 0"
  let pInfo = inspectPolynomialInfo polynomial
  let (varSet, dimension) = pInfo
  MyPrint.printLine "Dimension" $ show dimension
  let (solvable, reason) = isSolvable pInfo
  if solvable
    then do
        solveEquation polynomial
    else putStrLn ("This equation is not solvable. (" ++ reason ++ ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve expression
    _ -> showUsage

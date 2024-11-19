import Algebra
import Lexer
import MyPrint
import Parser (Equation (Equation), parseEquation)
import Polynomial
import Solver
import System.Environment
import System.IO

showUsage :: IO ()
showUsage = do
  path <- getProgName
  putStrLn (unwords ["Usage:", path, "<expression>"])

printEquation :: Equation -> IO ()
printEquation (Equation lhs rhs) = do
  let pl = transformToStandard lhs
  let pr = transformToStandard rhs
  MyPrint.printLine "Raw form" $ printPolynomial pl ++ " = " ++ printPolynomial pr

solve :: String -> IO ()
solve expression = do
  let tokens = lexer expression
  MyPrint.printLine "Tokens" $ show tokens

  let equation = parseEquation expression tokens
  MyPrint.printLine "AST" $ show equation
  printEquation equation

  let (Equation lhsAst rhsAst) = reduceEquation equation
  let polynomial = transformToStandard lhsAst
  MyPrint.printLine "Reduced form" $ printPolynomial polynomial ++ " = 0"

  let pInfo = inspectPolynomialInfo polynomial
  let maxD = maxDimension pInfo
  MyPrint.printLine "Dimension" $ show maxD

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

import Algebra
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Set qualified as Set
import Debug.Trace (trace)
import Lexer
import Parser
import Polynomial
import Solver
import System.Environment
import System.IO

showUsage :: IO ()
showUsage = do
  path <- getProgName
  putStrLn (unwords ["Usage:", path, "<expression>"])

getPolynomial :: String -> Polynomial
getPolynomial expression = trace ("[standard] -> " ++ polynomialSignature standard) standard
  where
    tokens = lexer expression
    equation = parseEquation tokens
    (Equation lhs rhs) = reduceEquation (trace ("[parser] -> " ++ show equation) equation)
    standard = transformToStandard (trace ("[reduce] -> " ++ show lhs ++ " = " ++ show rhs) lhs)

solve :: String -> IO ()
solve expression = do
  let polynomial = getPolynomial expression
  putStrLn (polynomialSignature polynomial ++ " = 0")
  let pInfo = inspectPolynomialInfo polynomial
  let (varSet, degree) = pInfo
  putStrLn ("Variables: [" ++ List.intercalate ", " (Set.toList varSet) ++ "]")
  putStrLn ("Polynomial degree: " ++ show degree)
  let (solvable, reason) = isSolvable pInfo
  if solvable
    then do
        putStrLn "This equation is solvable."
        solveEquation polynomial
    else putStrLn ("This equation is not solvable. (" ++ reason ++ ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve expression
    _ -> showUsage

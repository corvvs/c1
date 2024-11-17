import Algebra
import Data.ByteString qualified as BS
import Debug.Trace (trace)
import Distribution.PackageDescription (Executable (Executable))
import Lexer
import Parser
import Polynomial
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
  let (solvable, reason) = isSolvable polynomial
  if solvable
    then putStrLn "This equation is solvable."
    else putStrLn ("This equation is not solvable. (" ++ reason ++ ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve expression
    _ -> showUsage

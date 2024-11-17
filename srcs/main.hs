import Algebra
import Data.ByteString qualified as BS
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

solve :: String -> IO ()
solve expression = do
  let tokens = lexer expression
  print ("[lexer] " ++ show expression ++ " -> " ++ show tokens)
  let equation = parseEquation tokens
  print ("[parser] -> " ++ show equation)
  let (Equation lhs rhs) = reduceEquation equation
  print ("[reduce] -> " ++ show lhs ++ " = " ++ show rhs)
  let standard = transformoToStandard lhs
  print ("[standard] -> " ++ polynomialSignature standard)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve expression
    _ -> showUsage

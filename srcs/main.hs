import Data.ByteString qualified as BS
import System.IO
import Distribution.PackageDescription (Executable(Executable))
import System.Environment

import Lexer

showUsage :: IO ()
showUsage = do
    path <- getProgName;
    putStrLn (unwords ["Usage:", path, "<expression>"])

solve :: String -> IO()
solve expression = do
  let tokens = lexer expression
  print tokens

main :: IO ()
main = do
  args <- getArgs;
  case args of
    [expression]  -> solve expression
    _             -> showUsage


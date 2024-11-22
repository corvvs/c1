module Main (main) where

import System.Exit
import Lexer
import MyPrint
import Parser
import Polynomial
import Solver 
import System.Environment
import qualified Data.Text as T
import Control.Monad.Except
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)

showUsage :: IO ()
showUsage = do
  path <- getProgName
  putStrLn (unwords ["Usage:", path, "<expression>"])

solve :: T.Text -> IO ()
solve expression = do -- IOコンテキスト
  result <- runExceptT $ do -- ExceptT(ExceptTT)コンテキスト

    tokens <- lexer expression
    liftIO $ MyPrint.printLine "Tokens" $ T.pack $ show tokens

    equation <- parseEquation expression tokens
    liftIO $ MyPrint.printLine "Equation AST" $ T.pack $ show equation

    lhs <- reduceEquation equation
    liftIO $ MyPrint.printLine "Reduced form" $ T.concat [printPolynomial lhs, T.pack " = 0"]

    result <- solveEquation lhs
    liftIO $ result

  case result of
      Left err -> do
        TIO.putStrLn $ err
        exitWith (ExitFailure 1)
      Right _ -> return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expression] -> solve (T.pack expression)
    _ -> showUsage

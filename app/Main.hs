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

    -- 入力文字列からトークン列を生成
    tokens <- lexEquation expression
    liftIO $ MyPrint.printLine "Tokens" $ T.pack $ show tokens

    -- トークン列から方程式ASTを生成
    equation <- parseEquation expression tokens
    liftIO $ MyPrint.printLine "Equation AST" $ T.pack $ show equation

    -- 方程式ASTを多項式構造体に変換
    lhs <- reduceEquation equation
    liftIO $ MyPrint.printLine "Reduced form" $ T.concat [printPolynomial lhs, T.pack " = 0"]

    -- 多項式構造体をもとに方程式の解を求める
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

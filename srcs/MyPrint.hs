module MyPrint (printLine, showNumber) where

import Text.Printf

printLine :: String -> String -> IO ()
printLine = printf "[%-12s] %s\n"

showNumber :: Double -> String
showNumber n
  | n > 0 = "+ " ++ show n
  | n < 0 = "- " ++ show (abs n)
  | otherwise = show (abs n)

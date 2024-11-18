module MyPrint (printLine) where

import Text.Printf

printLine :: String -> String -> IO ()
printLine = printf "[%-12s] %s\n"

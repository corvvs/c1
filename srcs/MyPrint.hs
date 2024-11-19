module MyPrint (printLine, showNumber, emphasis) where

import Text.Printf

printLine :: String -> String -> IO ()
printLine = printf "[%-12s] %s\n"

showNumber :: Double -> String
showNumber n
  | n > 0 = "+ " ++ show n
  | n < 0 = "- " ++ show (abs n)
  | otherwise = show (abs n)

startEmphasis :: String
startEmphasis = "\ESC[01;31m"

endEmphasis :: String
endEmphasis = "\ESC[39;49m\ESC[0m"

emphasis :: String -> (Int, Int) -> String
emphasis str range = mainBody ++ "\n" ++ subBody
  where
    pre = take (fst range) str
    target = take (snd range - fst range) (drop (fst range) str)
    post = drop (snd range) str
    mainBody = pre ++ startEmphasis ++ target ++ endEmphasis ++ post

    preSpc = concat [" " | _ <- [1 .. length pre]]
    targetHat = concat ["^" | _ <- [1 .. length target]]
    postSpc = concat [" " | _ <- [1 .. length post]]
    subBody = preSpc ++ targetHat ++ postSpc

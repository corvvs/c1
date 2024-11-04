import Data.ByteString qualified as BS
import System.IO
import Distribution.PackageDescription (Executable(Executable))
import System.Environment

showUsage :: IO ()
showUsage = do
    path <- getExecutablePath;
    putStrLn (unwords ["Usage:", path, "<expression>"])

main :: IO ()
main = do
    args <- getArgs;
    case args of
        [expression] -> do
            putStrLn "ok"
        _ -> showUsage


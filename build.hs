#!/usr/bin/env stack

import System.Directory
import System.Process
import System.Exit
import System.FilePath.Posix

callCmd :: String -> [String] -> IO ()
callCmd cmd args = do
    (exitCode, out, err) <- readProcessWithExitCode cmd args []
    case exitCode of
        ExitSuccess -> putStrLn out
        (ExitFailure _) -> error err

stackBuild = callCmd "stack" ["build"]
stackInstall t = callCmd "stack" ["install", "--local-bin-path", t]

main :: IO ()
main = do
    stackBuild
    createDirectoryIfMissing False "target"
    stackInstall "target"
    files <- listDirectory "shaders"
    setCurrentDirectory "shaders"
    shaders <- mapM makeAbsolute files
    setCurrentDirectory ".."
    mapM_ (\src -> copyFile src ("target/" ++ (takeFileName src))) shaders

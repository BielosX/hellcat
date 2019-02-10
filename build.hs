#!/usr/bin/env stack

import System.Directory
import System.Process
import System.Exit
import System.FilePath.Posix

_callCmd :: String -> [String] -> ((String,String) -> String) -> IO ()
_callCmd cmd args f = do
    (exitCode, out, err) <- readProcessWithExitCode cmd args []
    case exitCode of
        ExitSuccess -> putStrLn out
        (ExitFailure _) -> error $ f (out, err)

callCmd cmd args = _callCmd cmd args snd

stackBuild = callCmd "stack" ["build"]
stackInstall t = callCmd "stack" ["install", "--local-bin-path", t]

checkShaders :: [FilePath] -> IO ()
checkShaders = mapM_ glslangValidator

glslangValidator f = _callCmd "glslangValidator" [f] fst

main :: IO ()
main = do
    stackBuild
    createDirectoryIfMissing False "target"
    stackInstall "target"
    files <- listDirectory "shaders"
    setCurrentDirectory "shaders"
    shaders <- mapM makeAbsolute files
    checkShaders shaders
    setCurrentDirectory ".."
    mapM_ (\src -> copyFile src ("target/" ++ (takeFileName src))) shaders

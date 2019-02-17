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

_checkShaders :: [FilePath] -> IO ()
_checkShaders = mapM_ glslangValidator

listAbs :: FilePath -> IO [FilePath]
listAbs path = do
    files <- listDirectory path
    setCurrentDirectory path
    r <- mapM makeAbsolute files
    setCurrentDirectory ".."
    return r

checkShaders :: IO ()
checkShaders = do
    shaders <- listAbs "shaders"
    _checkShaders shaders

glslangValidator f = _callCmd "glslangValidator" [f] fst

copyAll :: FilePath -> FilePath -> IO ()
copyAll from to = do
    absContent <- listAbs from
    mapM_ (\src -> copyFile src ((to ++ "/") ++ (takeFileName src))) absContent

main :: IO ()
main = do
    stackBuild
    createDirectoryIfMissing False "target"
    stackInstall "target"
    checkShaders
    copyAll "shaders" "target"
    copyAll "resources" "target"

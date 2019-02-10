module Main where

import Control.Monad.Except

import Lib

main :: IO ()
main = do
    r <- runExceptT someFunc
    case r of
        (Left e) -> putStrLn e
        (Right _) -> return ()

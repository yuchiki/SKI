module Main where

import           Repl
import           System.Directory

main :: IO ()
main = do
    c <- getCurrentDirectory
    h <- getHomeDirectory
    print c
    print h
    putStrLn ""
    initRepl

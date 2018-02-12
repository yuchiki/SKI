{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-} -- this line is needed for Doctests. The reason must be researched later.

module Repl.Internal where

import Control.Exception.Safe(catch, IOException)
import           Control.Monad
import           Core                   (Env, Statement (..), empty, eval,
                                         parse, showEnv, update)
import           Data.Either
import qualified SKILibrary
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.String     as ParsecS
import           Util
import Text.Printf(printf)
import HereDoc(heredoc)

type FileName = String

initRepl :: IO ()
initRepl = do
    hSetBuffering stdin LineBuffering
    putStr initialMessage
    repl ([], readLibrary empty SKILibrary.stdlib)

repl :: Info -> IO ()
repl info@(ls, e) = do
    prompt ls
    input <- trim <$> getLine
    when (null input) $ repl info
    newEi <- case Text.Parsec.parse command "" input of
        Right Help -> do
            putStr helpMessage
            return info
        Right Show -> do
            putStr $ showEnv e
            return info
        Right Statement -> readStatement info input
        Left _ -> do
            putStrLn $ errStr "Command parse error."
            putStrLn $ errStr "This message suggests an internal error in our command parser."
            return info
    repl newEi

readLibrary :: Env -> String -> Env
readLibrary env =
    foldl (\e (Assignment i t) ->  update i t e ) env . rights . map Core.parse . lines

type Libraries = [String]
type Info = (Libraries, Env)

prompt :: Libraries -> IO ()
prompt ls = do
    putStr . concat . foldr (\e a -> e : " " : a) ["SKI>"] $ ls
    hFlush stdout

readStatement :: Info -> String -> IO Info
readStatement (ei@(ls, e)) input =
    case Core.parse input of
        Left _                 -> do
            putStrLn $ errStr "could not parse."
            return ei
        Right (Import libname) -> openLibrary libname ei
        Right (Assignment s t) -> do
            putStrLn $ s @@@ "=" @@@ t
            return (ls, update s t e)
        Right (RawTerm t)      -> do
            mapM_ print $ saturate (eval e) t
            return ei

-- |
-- >>> info <- openLibrary "sampleLib" ([], empty)
-- ...
-- >>>info
-- (["sampleLib"],fromList [("plus",s i (k succ))])
openLibrary :: FileName -> Info -> IO Info
openLibrary libname (info@(ls, env)) = do
        contents <- hGetContents =<< openFile (convertToLPath libname) ReadMode
        putStrLn . okStr $ printf "%s loaded." libname
        return (libname : ls, readLibrary env contents)
    `catch`
    \(_ :: IOException) -> do
        putStrLn . errStr $ printf "cannot open %s." libname
        return info

convertToLPath:: String -> String
convertToLPath = printf "standardLibrary/%s.ski"

{- BNF
  ? | help | :s | :show | statement
-}

data Command = Help | Show | Statement

command :: ParsecS.Parser Command
command =
    spaces *>
    try (char '?' *> spaces *> eof ) *> return Help <|>
    try (string "help" *> spaces *> eof) *> return Help <|>
    try (string ":s" *> spaces *> eof) *> return Show <|>
    try (string ":show" *> spaces *> eof) *> return Show <|>
    return Statement

initialMessage :: String
initialMessage = [heredoc|
    SKI 0.1.0.0
    Type '?' to show help.
|]

helpMessage :: String
helpMessage = [heredoc|
    ?                         : show help
    :s                        : show definitions
    <term>                    : evaluate term
    import <library>          : import library
    let <identifier> = <term> : define term
|]

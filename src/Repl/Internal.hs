{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-} -- this line is needed for Doctests. The reason must be researched later.

module Repl.Internal where

import Control.Exception.Safe(catch, IOException)
import           Control.Monad
import           Core                   (Env, Statement (..), empty, eval,
                                         parse, showEnv, update, Term)
import           Data.Either
import qualified SKILibrary
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.String     as ParsecS
import           Util
import Data.List(intercalate)
import Text.Printf(printf)
import HereDoc(heredoc)
import Control.Monad.Loops

type FileName = String

initRepl :: IO ()
initRepl = do
    hSetBuffering stdin LineBuffering
    putStr initialMessage
    iterateM_ repl $ addLibrary "SKI" SKILibrary.stdlib emptyInfo

repl :: Info -> IO Info
repl info@(ls, e) = do
    putStrF $ prompt ls
    input <- trim <$> getLine
    if null input then return info
    else case Text.Parsec.parse command "" input of
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

addLibrary :: String -> String -> Info -> Info
addLibrary name contents (ls, env) = (name : ls, readLibrary env contents)

readLibrary :: Env -> String -> Env
readLibrary env =
    foldl (\e (Assignment i t) ->  update i t e ) env . rights . map Core.parse . lines

type Libraries = [String]
type Info = (Libraries, Env)

emptyInfo :: Info
emptyInfo = ([], empty)

updateAssign :: String -> Term -> Info -> Info
updateAssign s t (ls, env) = (ls, update s t env)

-- |
-- >>> prompt ["libA", "libB", "libC", "SKI"]
-- "libA libB libC SKI>"
prompt :: Libraries -> String
prompt = (++ ">") . unwords

readStatement :: Info -> String -> IO Info
readStatement (ei@(_, e)) input =
    case Core.parse input of
        Left _                 -> do
            putStrLn $ errStr "could not parse."
            return ei
        Right (Import libname) -> openLibrary libname ei
        Right (Assignment s t) -> do
            putStrLn $ printf "%s = %s" s (show t)
            return $ updateAssign s t ei
        Right (RawTerm t)      -> do
            mapM_ print $ saturate (eval e) t
            return ei

-- |
-- >>> info <- openLibrary "sampleLib" ([], empty)
-- ...
-- >>>info
-- (["sampleLib"],fromList [("plus",s i (k succ))])
openLibrary :: FileName -> Info -> IO Info
openLibrary libname info = do
        contents <- hGetContents =<< openFile (convertToLPath libname) ReadMode
        putStrLn . okStr $ printf "%s loaded." libname
        return $ addLibrary libname contents info
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

{-# LANGUAGE ScopedTypeVariables #-}
module Repl (repl, initRepl) where
import qualified Control.Exception.Safe as Exception
import           Control.Monad
import           Core                   (Env, Statement (..), empty, eval,
                                         parse, showEnv, update)
import           Data.Either
import qualified SKILibrary
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.String     as ParsecS
import           Util

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

initRepl :: IO ()
initRepl = do
  hSetBuffering stdin LineBuffering
  putStrLn "    SKI 0.1.0.0"
  putStrLn "Type '?' to show help."
  repl ([], readLibrary empty SKILibrary.stdlib)

readLibrary :: Env -> String -> Env
readLibrary e0 =
  foldl (\e (Assignment i t) ->  update i t e ) e0 . rights . map Core.parse . lines

type Libraries = [String]
type ExecutionInfo = (Libraries, Env)

prompt :: Libraries -> IO ()
prompt ls = do
  putStr . concat . foldr (\e a -> e : " " : a) ["SKI>"] $ ls
  hFlush stdout

repl :: ExecutionInfo -> IO ()
repl ei@(ls, e) = do
  prompt ls
  input <- trim <$> getLine
  when (null input) $ repl ei
  case Text.Parsec.parse command "" input of
    Right Help -> do
      putStrLn "?                         : show help"
      putStrLn ":s                        : show definitions"
      putStrLn "<term>                    : evaluate term"
      putStrLn $ italic "import" @@@ "<library>          : import library"
      putStrLn $ italic "let" @@@ "<identifier>" @@@ italic "=" @@@ "<term> : define term"
      repl ei
    Right Show -> do
      putStr $ showEnv e
      repl ei
    Right Statement -> readStatement ei input
    Left _ -> Prelude.putStrLn $ errStr "Command parse error."

readStatement :: ExecutionInfo -> String -> IO ()
readStatement (ei@(ls, e)) input =
  case Core.parse input of
    Left _                 -> do
      putStrLn $ errStr "could not parse."
      repl ei
    Right (Import libname) -> openLibrary libname ei
    Right (Assignment s t) -> do
      putStrLn $ s @@@ "=" @@@ t
      repl (ls, update s t e)
    Right (RawTerm t)      -> do
      mapM_ print $ saturate (eval e) t
      repl ei

openLibrary :: String -> ExecutionInfo -> IO ()
openLibrary libname (ei@(ls, e)) =
  do
    hLib <- openFile ("standardLibrary/" ++ libname ++ ".ski") ReadMode
    contents <- hGetContents hLib
    let newe = readLibrary e contents
    putStrLn . okStr $ libname @@@ "loaded."
    repl (libname : ls, newe)
  `Exception.catch`
  \(_ :: Exception.IOException) -> do
    putStrLn $ errStr $ "cannot open " ++ libname ++ "."
    repl ei

module Repl (repl, initRepl) where
import           Control.Monad
import           Core               (Env, Statement (..), empty, eval, parse,
                                     showEnv, update)
import           Data.Either
import qualified SKILibrary
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.String as ParsecS
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
  putStrLn "    SKI 0.1.0.0"
  putStrLn "Type '?' to show help."
  repl $ readLibrary empty SKILibrary.stdlib

readLibrary :: Env -> String -> Env
readLibrary e0 =
  foldl (\e (Assignment i t) ->  update i t e ) e0 . rights . map Core.parse . lines

repl :: Env -> IO ()
repl e = do
  putStr "SKI>"
  hFlush stdout
  input <- trim <$> getLine
  when (null input) $ repl e
  case Text.Parsec.parse command "" input of
    Right Help -> do
      putStrLn "?                         : show help"
      putStrLn ":s                        : show definitions"
      putStrLn "<term>                    : evaluate term"
      putStrLn $ concat [italic "let ", "<identifier>", italic " = ", "<term> : define term"]
      repl e
    Right Show -> do
      putStr $ showEnv e
      repl e
    Right Statement -> readStatement e input
    Left _ -> Prelude.putStrLn $ errStr "Command parse error."

readStatement :: Env -> String -> IO ()
readStatement e input =
  case Core.parse input of
    Left _                 -> do
      putStrLn $ errStr "could not parse."
      repl e
    Right (Assignment s t) -> do
      putStrLn $ concat [s, " = ", show t]
      repl $ update s t e
    Right (RawTerm t)      -> do
      mapM_ print $ saturateL (eval e) t
      repl e

saturate :: Eq a => (a -> a) -> a -> a
saturate f t
  |  f t == t = t
  | otherwise = saturate f (f t)

saturateL :: Eq a => (a -> a) -> a -> [a]
saturateL f t
  | f t == t = [t]
  | otherwise =  t : saturateL f (f t)


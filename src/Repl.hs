module Repl (repl, initRepl) where
import           Core      (Env, Statement (..), empty, eval, parse, update)
import           System.IO

initRepl :: IO ()
initRepl = repl empty

repl :: Env -> IO ()
repl e = do
  putStr ">"
  hFlush stdout
  input <- getLine
  case parse input of
    Left _                 -> do
      putStrLn "?"
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

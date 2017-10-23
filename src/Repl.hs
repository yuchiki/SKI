module Repl (repl) where
import           Core      (eval, parse)
import           System.IO

repl :: IO ()
repl = do
  putStr ">"
  hFlush stdout
  input <- getLine
  case parse input of
    Left _  -> putStrLn "?"
    Right t -> mapM_ print $ saturateL eval t
  putStrLn ""
  repl

saturate :: Eq a => (a -> a) -> a -> a
saturate f t
  |  f t == t = t
  | otherwise = saturate f (f t)

saturateL :: Eq a => (a -> a) -> a -> [a]
saturateL f t
  | f t == t = [t]
  | otherwise =  t : saturateL f (f t)

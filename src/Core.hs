module Core
    ( eval, parse
    ) where

import           Text.Parsec        (char, eof, letter, many, many1, spaces,
                                     (<|>))
import qualified Text.Parsec        as Parsec (parse)
import           Text.Parsec.String

data Term =
     Atom String
    | App Term Term
    deriving (Eq)

instance Show Term where
    show (Atom s)          = s
    show (t1 `App` Atom s) = show t1 ++ " " ++ s
    show (t1 `App` t2)     = show t1 ++ " (" ++ show t2 ++ ")"

{- term BNF
    term :: = argument | term argument
    argument = identifer | (term)
    identifier
-}

parse = Parsec.parse top ""

top :: Parser Term
top = term <* eof

term :: Parser Term
term = foldl1 App <$> many1 argument

argument :: Parser Term
argument =
    spaces *> (char '(' *> term <* char ')' <|> identifier) <* spaces

identifier :: Parser Term
identifier = Atom <$> many1 letter

eval :: Term -> Term
eval t@(Atom _)                            = t
eval (Atom "i" `App` t)                    = t
eval (Atom "k" `App` t `App` _)            = t
eval (Atom "s" `App` t1 `App` t2 `App` t3) = t1 `App` t3 `App` (t2 `App` t3)
eval (t1 `App` t2)                         = if eval t1 /= t1 then eval t1 `App` t2 else t1 `App` eval t2

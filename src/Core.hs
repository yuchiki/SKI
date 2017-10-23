module Core
    ( eval, parse, Statement(..), Env, empty, update
    ) where

import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (ParseError, char, eof, letter, many, many1,
                                     spaces, string, (<|>))
import qualified Text.Parsec        as Parsec (parse)
import           Text.Parsec.String

type Prog = [Statement]
data Statement = Assignment String Term | RawTerm Term

data Term =
     Atom String
    | App Term Term
    deriving (Eq)

instance Show Term where
    show (Atom s)          = s
    show (t1 `App` Atom s) = show t1 ++ " " ++ s
    show (t1 `App` t2)     = show t1 ++ " (" ++ show t2 ++ ")"

{- BNF
    top ::= statement eof
    statement ::= (asignment | term)
    asignment ::= let identifier = term
    term ::= argument | term argument
    argument ::= identifer | (term)
    identifier
-}

type Env = Map.Map String Term

update :: String -> Term -> Env -> Env
update = Map.insert

empty :: Env
empty = Map.empty

parse :: String -> Either ParseError Statement
parse = Parsec.parse top ""

top :: Parser Statement
top = statement <* eof

statement :: Parser Statement
statement = assignment <|> RawTerm <$> term

assignment :: Parser Statement
assignment = do
    string "let"
    spaces
    (Atom i) <- identifier
    spaces
    char '='
    t <- term
    return $ Assignment i t

term :: Parser Term
term = foldl1 App <$> many1 argument

argument :: Parser Term
argument = spaces *> (char '(' *> term <* char ')' <|> identifier) <* spaces

identifier :: Parser Term
identifier = Atom <$> many1 letter

eval :: Env -> Term -> Term
eval e t@(Atom s) = fromMaybe t $ Map.lookup s e
eval _ (Atom "i" `App` t)                    = t
eval _ (Atom "k" `App` t `App` _)            = t
eval _ (Atom "s" `App` t1 `App` t2 `App` t3) = t1 `App` t3 `App` (t2 `App` t3)
eval e (t1 `App` t2)                         = if eval e t1 /= t1 then eval e t1 `App` t2 else t1 `App` eval e t2

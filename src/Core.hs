module Core
    ( eval, parse, Statement(..), Env, empty, update, showEnv
    ) where

import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (ParseError, char, digit, eof, letter, many,
                                     many1, spaces, string, (<|>))
import qualified Text.Parsec        as Parsec (parse)
import           Text.Parsec.String

type Prog = [Statement]
data Statement = Import String | Assignment String Term | RawTerm Term deriving (Show)

data Term = Atom {get :: String} | CInt Int | App Term Term deriving (Eq)

instance Show Term where
    show (CInt i)          = show i
    show (Atom s)          = s
    show (t1 `App` Atom s) = show t1 ++ " " ++ s
    show (t1 `App` CInt i) = show t1 ++ " " ++ show i
    show (t1 `App` t2)     = show t1 ++ " (" ++ show t2 ++ ")"

{- BNF
    top ::= statement eof
    statement ::= (importLib | asignment | term)
    importLib ::= 'import' identifier
    asignment ::= let identifier = term
    term ::= argument | term argument
    argument ::= identifer | integer | (term)
    identifier
-}

type Env = Map.Map String Term

update :: String -> Term -> Env -> Env
update = Map.insert

empty :: Env
empty = Map.empty

showEnv :: Env -> String
showEnv e = concatMap (\(i, t) -> concat [pad 10 i, " = ", show t, "\n"] ) $ Map.toList e

parse :: String -> Either ParseError Statement
parse = Parsec.parse top ""

top :: Parser Statement
top = spaces *> statement <* eof

statement :: Parser Statement
statement = importLib <|> assignment <|> RawTerm <$> term

importLib :: Parser Statement
importLib = string "import" *> spaces *> ((Import . get) <$> identifier)

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
argument =
    spaces *> (
        char '(' *> term <* char ')' <|>
        CInt . read <$> many1 digit <|>
        identifier
    ) <* spaces

identifier :: Parser Term
identifier = Atom <$> many1 letter

eval :: Env -> Term -> Term
eval _ (CInt i) = foldl (flip ($)) (Atom "zero") $ replicate i (Atom "succ" `App`)
eval e t@(Atom s) = fromMaybe t $ Map.lookup s e
eval _ (Atom "i" `App` t)                    = t
eval _ (Atom "k" `App` t `App` _)            = t
eval _ (Atom "s" `App` t1 `App` t2 `App` t3) = t1 `App` t3 `App` (t2 `App` t3)
eval e (t1 `App` t2)                         = if eval e t1 /= t1 then eval e t1 `App` t2 else t1 `App` eval e t2

pad :: Int -> String -> String
pad i s = s ++ replicate (i - length s) ' '

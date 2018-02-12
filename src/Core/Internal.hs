module Core.Internal where

import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (ParseError, char, digit, eof, letter, many,
                                     many1, spaces, string, (<|>))
import qualified Text.Parsec        as Parsec (parse)
import           Text.Parsec.String
import           Util
import Text.Printf(printf)
import Control.Arrow(second)

type Prog = [Statement]
data Statement = Import String | Assignment String Term | RawTerm Term deriving (Show)

data Term = Atom {get :: String} | CInt Int | App Term Term deriving (Eq)

instance Show Term where
    show (CInt i)          = show i
    show (Atom s)          = s
    show (t1 `App` Atom s) = t1 @@@ s
    show (t1 `App` CInt i) = t1 @@@ i
    show (t1 `App` t2)     = t1 @@@ "(" @@ t2 @@ ")"

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
showEnv = concatMap (uncurry (printf "%10s=%s\n") . second show) . Map.toList

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

-- |
-- >>> Atom "a"
-- a
eval :: Env -> Term -> Term
eval _ (CInt i) = iterate (Atom "succ" `App`) (Atom "zero") !! i
eval e t@(Atom s) = fromMaybe t $ Map.lookup s e
eval _ (Atom "i" `App` t)                    = t
eval _ (Atom "k" `App` t `App` _)            = t
eval _ (Atom "s" `App` t1 `App` t2 `App` t3) = t1 `App` t3 `App` (t2 `App` t3)
eval e (t1 `App` t2)                         = if eval e t1 /= t1 then eval e t1 `App` t2 else t1 `App` eval e t2

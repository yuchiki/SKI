module Core.Internal where

import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (ParseError, char, digit, eof, letter,
                                     many1, notFollowedBy, choice, (<?>), oneOf, spaces, alphaNum, string, (<|>))
import qualified Text.Parsec        as Parsec (parse)
import           Text.Parsec.String
import           Util
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Printf(printf)
import Control.Arrow(second)

type Prog = [Statement]
data Statement = Import String | Assignment String Term | RawTerm Term deriving (Show, Eq)

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
statement = importLib <|> assignment <|> RawTerm <$> termParser

importLib :: Parser Statement
importLib =  do
    Token.reserved lexer "import"
    Import <$> Token.identifier lexer

assignment :: Parser Statement
assignment = do
    Token.reserved lexer "let"
    i <- Token.identifier lexer
    Token.reservedOp lexer "="
    t <- termParser
    return $ Assignment i t


identifier :: Parser Term
identifier = Atom <$> many1 letter

def = emptyDef{
    Token.commentStart = ""
    , Token.commentEnd = ""
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.opStart = oneOf "="
    , Token.opLetter = oneOf ""
    , Token.reservedOpNames = ["="]
    , Token.reservedNames = ["import", "let", "import", ":s"]
}

lexer = Token.makeTokenParser def

termParser :: Parser Term
termParser = buildExpressionParser [[appOp]] terms <?> "term"

appOp = Infix appOpBody AssocLeft
    where
        appOpBody = Token.whiteSpace lexer
            *> notFollowedBy (choice . map (Token.reservedOp lexer) $ ["="])
            *> return App

terms =
    CInt . fromIntegral <$> Token.integer lexer <|>
    Token.parens lexer termParser <|>
    Atom <$> Token.identifier lexer

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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE TemplateHaskell #-} -- this line is needed for Doctests. The reason must be researched later.

module Repl.Internal where

import Control.Exception.Safe(catch, IOException)
import           Core                   (Env, Statement (..), empty, eval,
                                         parse, showEnv, update, Term (..))
import           Data.Either
import qualified SKILibrary
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.String     as ParsecS
import           Util
import Text.Printf(printf)
import HereDoc(heredoc)
import Control.Monad.Loops
import Data.Maybe(fromJust)

type FileName = String
type Libraries = [String]
data Info = Info { getLibs :: Libraries, getEnv :: Env } deriving (Show)


initRepl :: IO ()
initRepl = do
    hSetBuffering stdin LineBuffering
    putStr initialMessage
    iterateM_ repl . fromJust $  addLibrary "SKI" SKILibrary.stdlib emptyInfo

repl :: Info -> IO Info
repl info = do
    putStrF $ prompt (getLibs info)
    input <- trim <$> getLine
    if null input then return info
    else case Text.Parsec.parse command "" input of
        Right Help -> do
            putStr helpMessage
            return info
        Right Show -> do
            putStr $ showEnv (getEnv info)
            return info
        Right Statement -> readStatement info input
        Left _ -> do
            putStr . errStr $ [heredoc|
                Command parse error.
                This message suggests an internal error in our command parser.
            |]
            return info

readStatement :: Info -> String -> IO Info
readStatement info input =
    case Core.parse input of
        Left _                 -> do
            putStrLn $ errStr "could not parse."
            return info
        Right (Import libname) -> openLibrary libname info
        Right (Assignment s t) -> do
            putStrLn $ printf "%s = %s" s (show t)
            return $ updateAssign s t info
        Right (RawTerm t)      -> do
            mapM_ print $ saturate (eval (getEnv info)) t
            return info

-- |
-- >>> info <- openLibrary "sampleLib" emptyInfo
-- ...
-- >>> getLibs info
-- ["sampleLib"]
-- >>> getEnv info
-- fromList [("plus",s i (k succ))]
openLibrary :: FileName -> Info -> IO Info
openLibrary libname info = do
        contents <- hGetContents =<< openFile (convertToLPath libname) ReadMode
        let parseResult = addLibrary libname contents info
        case  parseResult of
            Just newInfo -> do
                putOkStrLn $ printf "%s loaded." libname
                return newInfo
            Nothing -> do
                putErrStrLn $ printf "grammatical error."
                return info
    `catch`
    \(_ :: IOException) -> do
        putStrLn . errStr $ printf "cannot open %s." libname
        return info

emptyInfo :: Info
emptyInfo = Info [] empty

updateAssign :: String -> Term -> Info -> Info
updateAssign s t info =
    let env = getEnv info in
    info { getEnv = update s t env}

addLibrary :: String -> String -> Info -> Maybe Info
addLibrary name contents info =
    let libs =  getLibs info in
    let env = getEnv info in
    readLibrary env contents >>= \x -> Just (info {getLibs = name : libs, getEnv = x})

-- |
-- >>> let env = update "x" (Atom "a") empty
-- >>> readLibrary env "let y=b\nlet z=c"
-- Just (fromList [("x",a),("y",b),("z",c)])
readLibrary :: Env -> String -> Maybe Env
readLibrary env =
    Just . foldl (\e (Right (Assignment i t)) ->  update i t e ) env . map  Core.parse . lines


-- |
-- >>> prompt ["libA", "libB", "libC", "SKI"]
-- "libA libB libC SKI>"
prompt :: Libraries -> String
prompt = (++ ">") . unwords

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

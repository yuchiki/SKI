module HereDoc(heredoc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

heredoc :: QuasiQuoter
heredoc = QuasiQuoter {
    quoteExp = stringE . trimIndent,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}

trimIndent :: String -> String
trimIndent "" = ""
trimIndent s = unlines $ map (drop indentAmount) ls 
    where
        ls = tail . lines $ s
        indentAmount = getIndent . head $ ls

getIndent :: String -> Int
getIndent = length . takeWhile (== ' ')
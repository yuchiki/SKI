{-# LANGUAGE QuasiQuotes #-}

module SKILibrary (stdlib) where

import HereDoc(heredoc)

stdlib :: String
stdlib = [heredoc|
    let cp = s i i
    let rev = s (k (s i)) k
    let zero = k i
    let succ = s (s (k s) k)
|]
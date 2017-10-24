module SKILibrary (stdlib) where

stdlib :: String
stdlib =
    "\
    \let cp = s i i \n\
    \let rev = s (k (s i)) k \n\

    \let zero = k i\n\
    \let succ = s (s (k s) k) \n\
    \"

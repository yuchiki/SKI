module SKILibrary (stdlib) where

stdlib :: String
stdlib =
    "\
    \let cp = s i i \n\
    \let rev = s (k (s i)) k \n\

    \let if = i \n\
    \let true = k \n\
    \let false = k i \n\
    \let not = s (s i (k f))(k t) \n\
    \let or = s i (k t) \n\
    \let and = s s (k (k f)) \n\

    \let zero = k i\n\
    \let succ = s (s (k s) k) \n\
    \let one = succ zero \n\
    \let two = succ one \n\
    \let three = succ two \n\
    \let four = succ three \n\
    \let five = succ four \n\
    \let six = succ five \n\
    \let seven = succ six \n\
    \let eight = succ seven \n\
    \let nine = succ eight \n\
    \let ten = succ nine \n\
    \"

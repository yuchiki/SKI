{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-} -- this line is needed for Doctests. The reason must be researched later.

module Repl (repl, initRepl) where

import Repl.Internal(repl, initRepl)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Util (decorate, errStr, italic, trim, okStr, (@@), (@@@)) where

import           Data.Char (isSpace)

type Decorator = String -> String

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
deColor :: Color -> Int
deColor Black   = 30
deColor Red     = 31
deColor Green   = 32
deColor Yellow  = 33
deColor Blue    = 34
deColor Magenta = 35
deColor Cyan    = 36
deColor White   = 37

data TextAttribute = Normal | Bold | Vague | Italic | Underscore | Reverse | Strike

deAttribute :: TextAttribute -> Int
deAttribute Normal     = 0
deAttribute Bold       = 1
deAttribute Vague      = 2
deAttribute Italic     = 3
deAttribute Underscore = 4
deAttribute Reverse    = 7
deAttribute Strike     = 9



decorate :: TextAttribute -> Color -> Decorator
decorate a c s = concat ["\ESC[", show $ deAttribute a, ";", show $ deColor c, "m", s, "\ESC[0;39m"]

errStr :: Decorator
errStr = decorate Normal Red

okStr :: Decorator
okStr = decorate Normal Green

italic :: Decorator
italic = decorate Italic White

trim :: String -> String
trim =  f . f
    where f = reverse . dropWhile isSpace


class Show a => Shon a where
    shon :: a -> String
    shon = show
instance Shon String where
    shon = id
instance Show a => Shon a

(@@) :: (Shon a, Shon b) => a -> b -> String
x @@ y = shon x ++ shon y
infixr 5 @@

(@@@) :: (Shon a, Shon b) => a -> b -> String
x @@@ y   = shon x ++ " " ++ shon y
infixr 5 @@@

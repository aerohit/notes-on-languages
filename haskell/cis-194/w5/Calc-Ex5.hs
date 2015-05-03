{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Exercise 5

instance Expr Program where
  lit (x::Integer) = [(PushI x)]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

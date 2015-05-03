-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map (\(x,_) -> x) fibPairs where
  fibPairs = iterate (\(x,y) -> (y,x+y)) (0,1)

-- Exercise 3
data Stream a = SCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show stream = show (take 20 (streamToList stream))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = SCons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (SCons x xs) = SCons (fn x) (streamMap fn xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = SCons x (streamFromSeed fn (fn x))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (1+) 0

{-rulerFn :: Integer -> Integer-}
{-rulerFn n = if (odd n) then 0 else (1 + rulerFn (n/2))-}
-- TODO
{-ruler :: Stream Integer-}

-- Exercise 6 TODO

-- Exercise 7 TODO

-- Declarations and variables.
x :: Int
x = 3

-- Boolean logic
-- Following are all valid expressions:
-- True && False
-- not (True || False)
-- 'a' == 'a'
-- 16 /= 4
-- "Haskell" > "C++"


-- Basic Functions
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

hailstone :: Integer -> Integer
hailstone n
  | isEven n  = n `div` 2
  | otherwise = 3 * n + 1

-- Pairs
p :: (Int, Char)
p = (3, 'c')

-- we can extract out the elements of a pair using pattern matching
sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

-- Functions and multiple arguments
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

-- Lists
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

hello :: [Char]
hello = "Hello"

-- Constructing lists
areEqual = [1, 2] == 1 : 2 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Functions on lists
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []       = []
sumEveryTwo (x:[])   = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs

-- Combining functions
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

-- Declarations and variables
x :: Int
x = 7

-- following will cause the repl to hang forever.
y :: Int
y = y + 1

-- Basic types

-- Machine sized integers
maxInt :: Int
maxInt = maxBound

-- Arbitrary precision integers
n :: Integer
n = 123342212420298412082798347219873892378234798237892378237489237483297328

-- Double precision
pi :: Double
pi = 3.1415

-- Single precision
e :: Float
e = 2.71

-- Booleans
true, false :: Bool
true  = True
false = False

-- Unicode characters
c :: Char
c = 'x'

-- Strings
s :: String
s = "Hello, Haskell!"

-- Boolean logic
trueAndFalse = True && False
negation     = not (True || False)

-- Defining basic functions
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- Pairs
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- Lists
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..10]
range2 = [2,4..100]

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']
hello2 :: [Char]
hello2 = "hello"

uptoFour = 1 : 2 : 3 : 4 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

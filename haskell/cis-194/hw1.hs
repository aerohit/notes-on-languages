-- Exercise 1

myreverse :: [Integer] -> [Integer]
myreverse []     = []
myreverse (x:xs) = myreverse xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = myreverse (toDigitsRev n)

-- Exercise 2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []         = []
doubleEveryOtherRev [x]        = [x]
doubleEveryOtherRev (x:(y:zs)) = x : (2 * y) : (doubleEveryOtherRev zs)

-- Definitely not the most optimal, but making do with what I know
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

-- Exercise 3

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumList (toDigits x) + (sumDigits xs)

-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `rem` 10 == 0

-- Exercise 5
type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

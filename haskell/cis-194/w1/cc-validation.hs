toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

reverse' :: [Integer] -> [Integer]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

toDigits :: Integer -> [Integer]
toDigits n = reverse' (toDigitsRev n)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []       = []
doubleEveryOtherRev (x:[])   = [2*x]
doubleEveryOtherRev (x:y:zs) = 2*x : y : doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse' (doubleEveryOtherRev (reverse' xs))

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOtherRev (toDigits n))) `mod` 10 == 0

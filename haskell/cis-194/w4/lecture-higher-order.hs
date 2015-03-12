-- Anonymous functions
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter (\x -> x > 100)

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (> 100)

-- Operator sections
-- (?y) is equivalent to the function \x -> x ? y,
-- and (y?) is equivalent to \x -> y ? x

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Folds
myfoldr :: z -> (a -> z -> z) -> [a] -> z
myfoldr z _ []      = z
myfoldr z fn (x:xs) = myfoldr (fn x z) fn xs

myfoldl :: z -> (z -> a -> z) -> [a] -> z
myfoldl z _ []      = z
myfoldl z fn (x:xs) = myfoldl (fn z x) fn xs

sum' :: [Integer] -> Integer
sum' = myfoldl 0 (+)

product' :: [Integer] -> Integer
product' = myfoldl 1 (*)

length' :: [a] -> Integer
length' = myfoldl 0 (\z _ -> z + 1)

and' :: [Bool] -> Bool
and' = myfoldl True (\z x -> z && x)

or' :: [Bool] -> Bool
or' = myfoldl False (\z x -> z || x)

any' :: (a -> Bool) -> [a] -> Bool
any' fn = or' . map fn

all' :: (a -> Bool) -> [a] -> Bool
all' fn = and' . map fn

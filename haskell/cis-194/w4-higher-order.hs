-- Anonymous functions
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' xs = filter (>100) xs

-- Function composition
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

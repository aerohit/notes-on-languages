-- Exercise 1

-- 1.1
fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

-- 1.2 == TODO
{-fun2' :: Integer -> Integer-}


-- Exercise 2
-- to test the implementation, it would be a good idea to check the
-- depth of each leaf and see that it's the same (off by 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

balancedInsert :: a -> Tree a -> Tree a
balancedInsert x Leaf             = Node 0 Leaf x Leaf
balancedInsert x (Node _ lt y rt) =
  let (lh, rh) = ((height lt), (height rt))
      (lt', rt') = if (lh == rh)
                   then ((balancedInsert x lt), rt)
                   else (lt, (balancedInsert x rt))
   in Node (1 + (max (height lt') (height rt'))) lt' y rt'

foldTree :: [a] -> Tree a
foldTree = foldr balancedInsert Leaf


-- Exercise 3

-- 3.1
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x fxs -> (f x) : fxs) []

-- 3.3
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f = foldr (flip f)

-- Exercise 4
strikedOut :: Int -> [Int]
strikedOut n = [n' | x <- [1..n], y <- [x..n], let n' = x + y + 2*x*y, n' <= n]

sieveSundaran :: Int -> [Int]
sieveSundaran n = map (\x -> 2*x + 1) $ filter (\x -> not (elem x allStrikedOut)) [1..n] where allStrikedOut = strikedOut n

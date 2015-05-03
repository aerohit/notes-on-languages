{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node l x r) = flattenTree l ++ [x] ++ flattenTree r

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\ls _ rs -> 1 + ls + rs)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\ls x rs -> x + ls + rs)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\ld _ rd -> 1 + max ld rd)

flattenTree' :: Tree a -> [a]
flattenTree' = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

-- Folding expressions :: TODO, read it again.

-- Monoids

class Monoid' m where
  mempty  :: m
  mappend :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

-- (<>) :: Monoid' m -> m -> m -> m
-- (<>) = mappend

instance Monoid' [a] where
  mempty  = []
  mappend = (++)

-- Defining monoids for addition and multiplication
-- To be honest, I don't know how the following works.

newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid' (Sum a) where
  mempty  = Sum 0
  mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid' (Product a) where
  mempty  = Product 1
  mappend = (*)

lst = [1, 2, 3, 4]
prod = getProduct . mconcat . map Product $ lst

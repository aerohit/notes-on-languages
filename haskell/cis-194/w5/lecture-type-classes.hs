{-# LANGUAGE FlexibleInstances #-}
-- Type classes

data Foo = F Int | G Char

instance Eq Foo where
  (F f1) == (F f2) = f1 == f2
  (G g1) == (G g2) = g1 == g2
  _      == _      = False

  foo1 /= foo2 = not (foo1 == foo2)

-- But GHC is smart to generate typeclasses for some of the classes on
-- it's own like:
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL :: Listable a => a -> Int
sumL x = sum (toList x)

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y

-- Recursion patterns
data IntList = Empty | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty        = Empty
mapIntList fn (Cons x xs) = Cons (fn x) (mapIntList fn xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList predicate (Cons x xs)
  | predicate x = Cons x (filterIntList predicate xs)
  | otherwise   = filterIntList predicate xs

foldlIntList :: (Int -> Int -> Int) -> Int -> IntList -> Int
foldlIntList _ zero Empty        = zero
foldlIntList fn zero (Cons x xs) = foldlIntList fn (fn zero x) xs


-- Polymorphism

data List' t = Empty' | Cons' t (List' t)

lst1 :: List' Int
lst1 = Cons' 3 (Cons' 5 (Cons' 2 Empty'))

lst2 :: List' Char
lst2 = Cons' 'x' (Cons' 'y' (Cons' 'z' Empty'))

lst3 :: List' Bool
lst3 = Cons' True (Cons' False Empty')

filterList :: (t -> Bool) -> List' t -> List' t
filterList _ Empty' = Empty'
filterList predicate (Cons' x xs)
  | predicate x = Cons' x (filterList predicate xs)
  | otherwise   = filterList predicate xs

mapList :: (a -> b) -> List' a -> List' b
mapList _ Empty'        = Empty'
mapList fn (Cons' x xs) = Cons' (fn x) (mapList fn xs)

data Maybe' t = Nothing' | Just' t

safeHead :: [t] -> Maybe' t
safeHead []     = Nothing'
safeHead (x:xs) = Just' x

data NonEmptyList t = NEL t [t]

nelToList :: NonEmptyList t -> [t]
nelToList (NEL x xs) = x:xs

listToNel :: [t] -> Maybe' (NonEmptyList t)
listToNel []     = Nothing'
listToNel (x:xs) = Just' (NEL x xs)

headNel :: NonEmptyList t -> t
headNel (NEL x _) = x

tailNel :: NonEmptyList t -> [t]
tailNel (NEL _ xs) = xs

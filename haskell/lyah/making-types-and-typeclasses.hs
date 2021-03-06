module Shapes
( Point (..)
, Shape (..)
, surface
, nudge
, baseCircle
, baseRectangle
) where

import qualified Data.Map as Map
-- Abstract Data Types

-- 'data' means that we're defining a new data type. The part
-- before the = denotes the type, which is Bool. The parts
-- after the = are value constructors.
data Bool' = False' | True' deriving (Eq, Ord)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r)                            = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

-- cocentricCircles = map (Circle (Point 10 20)) [4,5,6,6]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy                  = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle x y = Rectangle (Point 0 0) (Point x y)

-- Record Syntax

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show, Read, Eq)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
car = Car {company="Ford", model="Mustang", year=1967}

-- Type parameters
-- 'type constructors' can take types as parameters to produce new types.
data Maybe' a = Nothing' | Just' a deriving (Show, Ord, Eq)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vecMult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `vecMult` n = Vector (x*n) (y*n) (z*n)

scalarMult :: (Num a) => Vector a -> Vector a -> a
(Vector x1 y1 z1) `scalarMult` (Vector x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

-- Derived instances
-- typeclass is a sort of an interface that defines some behavior.
-- Some examples are: Eq, Ord, Enum, Bounded, Show, Read

-- since Person is now in Eq, we can use it as the a for all
-- functions that have a class constraint of Eq a in their type
-- signature, such as elem.

mika = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

mon      = minBound :: Day
tue      = succ Monday
weekdays = [Monday .. Friday]
alldays  = [minBound .. maxBound] :: [Day]

-- Type synonyms

type Days = [Day]
alldays'  = [minBound .. maxBound] :: Days

type PhoneNumber = String
type Name        = String
type PhoneBook   = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [("betty","555-2938")
  ,("bonnie","452-2928")
  ,("patsy","493-2928")
  ,("lucille","205-2928")
  ,("wendy","939-8282")
  ,("penny","853-2492")
  ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name,number) `elem` book

type AssocList k v = [(k, v)]

getKey :: (Eq k) => k -> AssocList k v -> Maybe v
getKey _ [] = Nothing
getKey k ((k',v'):kvs) = if k == k'
                            then Just v'
                            else getKey k kvs

-- Partially applied typeclasses
type IntAssocList v = AssocList Int v
type IntMap = Map.Map Int

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show, Read)

data LockerState = Taken | Free deriving (Show, Eq)
type Code        = String
type LockerMap   = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap =
  case Map.lookup lockerNumber lockerMap of
       Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
       Just (state, code) -> if state == Taken
                                then Left $ "Locker " ++ show lockerNumber ++ " isn't available"
                                else Right code

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Recursive data structures
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

aList :: List Int
aList = 3 `Cons` (4 `Cons` (5 `Cons` Empty))

-- Defining infix constructors
infixr 5 :-:
data List' a = Empty' | a :-: List' a deriving (Show, Read, Ord, Eq)

bList :: List' Int
bList = 3 :-: 4 :-: 5 :-: Empty'

infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty' .++ ys     = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

bbList = bList .++ bList


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y l r)
    | x == y = Node y l r
    | x < y  = Node y (treeInsert x l) r
    | x > y  = Node y l (treeInsert x r)


nums = [8,6,4,1,7,3,5]
numsTree :: Tree Int
numsTree = foldr treeInsert EmptyTree nums

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y l r)
  | x == y = True
  | x < y  = treeElem x l
  | x > y  = treeElem x r

class Eq' equatable where
  (.==) :: equatable -> equatable -> Bool
  (./=) :: equatable -> equatable -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)

data TrafficLight = Red | Yellow | Green

instance Eq' TrafficLight where
  Red    .== Red    = True
  Green  .== Green  = True
  Yellow .== Yellow = True
  _      .== _      = False

instance Show TrafficLight where
  show Red    = "Red Light"
  show Green  = "Green Light"
  show Yellow = "Yellow Light"

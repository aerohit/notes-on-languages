-- Enumeration types
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- Pattern matching
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall' :: Thing -> Bool
isSmall' Ship = False
isSmall' King = False
isSmall' _    = True

-- Beyond Enumerations to ADTs

data FailableDouble = Failure
                    | OK Double
  deriving Show

ex1 = Failure
ex2 = OK 3.14

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _u)         = n ++ ", you're not my kind of person!"

-- case expressions
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

-- Recursive data types
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

data IntTree = Leaf Char
             | Node IntTree Int IntTree
  deriving Show

tree :: IntTree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

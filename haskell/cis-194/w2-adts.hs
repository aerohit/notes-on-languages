data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax, King, Cabbage]

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

data FailableDouble = Failure
                    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

getAge :: Person -> Int
getAge (Person _ a _) = a

describe :: Person -> String
describe p@(Person n _ _) = "The name of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame"

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                        Failure -> 0
                        OK d -> d

data IntList = Empty | Cons Int IntList

intListProduct :: IntList -> Int
intListProduct Empty      = 1
intListProduct (Cons x l) = x * intListProduct l

data Tree = Leaf Char
          | Node Tree Int Tree

negate x = if x
              then True
              else False

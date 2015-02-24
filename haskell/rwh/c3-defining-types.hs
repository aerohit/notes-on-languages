data BookInfo = Book Int String [String]
                deriving Show

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address    = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

-- Enumeration
data Vibgyor = Violet
             | Indigo
             | Blue
             | Green
             | Yellow
             | Orange
             | Red
               deriving (Eq, Show)

type Point = (Double, Double)
data Shape = Circle Point Double
           | Poly [Point]

myNot True  = False
myNot False = True

third (a, b, c) = c

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- Record Syntax
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

-- Parametrized Types
data MyMaybe a = MyJust a
               | MyNothing

someBool = MyJust True

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Node (Tree a) a (Tree a)
            | Emtpy
              deriving (Show)

-- Reporting erros
mySecond :: [a] -> a
mySecond xs = if null (tail xs)
                then error "list too short"
                else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                  then Nothing
                  else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

-- Local variables
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if newBalance < reserve
                           then Nothing
                           else Just newBalance

lend2 amount balance = if newBalance < reserve
                         then Nothing
                         else Just newBalance
     where reserve = 100
           newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"

fromMaybe defval wrapped =
    case wrapped of
      Nothing    -> defval
      Just value -> value

nodesAreSame (Node a _ _) (Node b _ _)
  | a == b       = Just a
nodesAreSame _ _ = Nothing

niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n-1) xs

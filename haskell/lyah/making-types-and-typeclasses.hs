module Shapes
( Point (..)
, Shape (..)
, surface
, nudge
, baseCircle
, baseRectangle
) where

-- 'data' means that we're defining a new data type. The part
-- before the = denotes the type, which is Bool. The parts
-- after the = are value constructors.
data Bool' = True' | False'

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r)                            = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

cocentricCircles = map (Circle (Point 10 20)) [4,5,6,6]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy                  = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle x y = Rectangle (Point 0 0) (Point x y)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
car = Car {company="Ford", model="Mustang", year=1967}

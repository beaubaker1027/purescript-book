module Test.MySolutions where

import Prelude
import Math(pi)
import Data.Picture
  ( Bounds
  , Picture
  , Point(Point)
  , Shape(Circle, Rectangle, Line, Text)
  , bounds
  , getCenter
  , getX
  , getY
  , intersect
  , origin
  )
import Data.Picture as DataP
import Data.Maybe(Maybe(Just, Nothing))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial( n - 1 )

binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial _ 0 = 1
binomial x y  | x < y     = 0
              | otherwise  = (factorial x) / ((factorial y) * (factorial (x - y)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k  | n == k      = 1
            | n < k       = 0
            | otherwise   =(pascal (n-1) k )+ (pascal (n - 1) (k - 1))

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x}} { address: { city: y}}
  | x == y = true
  | otherwise = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton a [b] = b
fromSingleton a _   = a

-- data Point = Point
--   { x :: Number
--   , y :: Number
--   }

-- data Shape
--   = Circle Point Number
--   | Rectangle Point Number Number
--   | Line Point Point
--   | Text Point String
--   | Clipped Picture Point Number Number

-- origin :: Point
-- origin = Point { x, y }
--   where
--     x = 0.0
--     y = 0.0

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle c r) = Circle origin r
centerShape (Rectangle c w h) = Rectangle origin w h
centerShape line@(Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x - deltaX, y: s.y - deltaY })
    (Point { x: e.x - deltaX, y: e.y - deltaY })
  )
  where
  delta = getCenter line
  deltaX = getX delta
  deltaY = getY delta
centerShape (Text loc text) = Text origin text

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x * i, y: s.y * i })
    (Point { x: e.x * i, y: e.y * i })
  )
scaleShape i text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _             = Nothing

area :: Shape -> Number
area (Circle _ r)       = pi * r * r
area (Rectangle _ l w)  = l * w
area _                  = 0.0


-- Kept giving me unknown data constructor errors
data ShapeExt
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBounds :: ShapeExt -> Bounds
shapeBounds (Clipped pic pt w h) = intersect (bounds pic) (DataP.shapeBounds (Rectangle pt w h))
shapeBounds (Shape shape) = DataP.shapeBounds shape

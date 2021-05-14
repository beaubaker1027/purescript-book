module Test.MySolutions where

import Prelude
import Global (readFloat)
import Math (sqrt, pi, e)

diagonal::Number -> Number -> Number
diagonal w h = sqrt $ add (mul w w) (mul h h)

circleArea::Number -> Number
circleArea r = pi * r * r

addE::String -> Number
addE s = readFloat s + e

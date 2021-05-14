module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hash, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)

-- Note to reader: Add your solutions to this file
data Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point p) = "(" <> show p.x <> ", " <> show p.y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }
derive instance newtypeComplex :: Newtype Complex _

instance showComplex :: Show Complex where
  show ( Complex c ) =
    let
      optional_plus
        | c.imaginary >= 0.0 = "+"
        | otherwise          = ""
    in
      show c.real <> optional_plus <> show c.imaginary <> "i"

derive instance eqComplex :: Eq Complex


instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  mul = over2 Complex
                \ { real: r1, imaginary: i1}
                  { real: r2, imaginary: i2}
                  ->
                  { real:      r1 * r2 - i1 * i2
                  , imaginary: r1 * i2 + r2 * i1
                  }
  one = wrap one
  zero = wrap zero

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty v a) = "NonEmpty: " <> show v <> show a

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty v1 l1) (NonEmpty v2 l2) = NonEmpty v1  ( l1 <> [ v2 ] <> l2 )

derive instance functorNonEmpty :: Functor (NonEmpty)

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite v1) (Finite v2) = compare v1 v2

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr func st (NonEmpty v l)  = foldr func st ([v] <> l)
  foldl func st (NonEmpty v l)  = foldl func st ([v] <> l)
  foldMap func (NonEmpty v l)   = foldMap func ([v]<>l)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr func st (OneMore val more) = func val lastB
    where
    lastB = foldr func st more
  foldl func st (OneMore val more) = foldl func firstB more
    where
    firstB = (func st val)
  foldMap func (OneMore val more) = (func val) <> (foldMap func more)

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just m -> m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply a) b = a * b

-- These may also be written manualy
derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = power s n

instance actionArray :: Action m a => Action m (Array a) where
  act m arr = map (act m) arr

newtype Self m
  = Self m

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

-- These may also be written manualy
derive newtype instance showSelf :: Show m => Show (Self m)
derive newtype instance eqSelf :: Eq m => Eq (Self m)
derive newtype instance semigroupSelf :: Semigroup m => Semigroup (Self m)
derive newtype instance monoidSelf :: Monoid m => Monoid (Self m)

instance repeatActionMultSelf :: Action (Self Multiply) Int where
  act (Self (Multiply m)) s = m * s

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr =
  let
    hashAndValEqual a b = hashEqual a b && a == b
  in
    length arr /= (length $ nubByEq hashAndValEqual arr)

newtype Hour
  = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour h) = hash $ mod h 12

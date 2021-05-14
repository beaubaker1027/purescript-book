module Test.MySolutions where

import Prelude
import Effect (Effect)
import Effect.Exception (throwException, error)
import Data.Array
import Data.List(List(..), (:))
import Data.Maybe
import Data.Traversable

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third xs = do
  woZeroIndex <- tail xs
  woOneIndex <- tail woZeroIndex
  payload  <- head woOneIndex
  pure payload

possibleSums :: Array Int -> Array Int
possibleSums xs = nub $ sort $ foldM ( \acc a -> [ acc, acc + a ] ) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil

filterM f (x : xs) = do
  b <- f x
  xs' <- filterM f xs
  pure if b then x : xs' else xs'

exceptionDivide :: Number -> Number -> Effect Number
exceptionDivide x y = case y of
  0.0 -> throwException $ error "cant divide by zero"
  _ -> pure ( x / y )

module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, nub, filter, range, find, last, concatMap, head)
import Data.Foldable (length, foldl)
import Data.Path (Path, isDirectory, ls, size, filename)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Test.Examples (factors, allFiles)

-- import Test.NoPeeking.Solutions (keepNonNegative, keepNonNegativeRewrite)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven( x `mod` 2 )

countEven :: Array Int -> Int
-- this is better
countEven ints = (length $ filter isEven ints)
-- countEven ints = countEven' ints 0
--   where
--   countEven' :: Array Int -> Int -> Int
--   countEven' [] count = count
--
--   countEven' ints' count = countEven' (fromMaybe [] (tail ints')) $ add count $ maybe 0 oneIfEven $ head ints'

squared :: Array Number -> Array Number
squared ints = map (\x -> x * x) ints

keepNonNegative :: Array Number -> Array Number
keepNonNegative values = filter (\x -> x >= 0.0) values

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite values = (\x -> x >= 0.0) <$?> values

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct left right = do
  a_ <- left
  b_ <- right
  pure [ a_, b_ ]

triples :: Int -> Array (Array Int)
triples n = do
  i <- range 1 n
  j <- range i n
  k <- range j n
  guard $ ((i * i) + (j * j)) == (k * k)
  pure [ i, j, k ]


factorize :: Int -> Array Int
factorize n = concatMap ( filter isPrime ) $ factors n

allTrue :: Array Boolean -> Boolean
allTrue bools = foldl ( \xs x -> xs && x ) true bools

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse bools = foldl (\xs x -> [x]<> xs) [] bools

onlyFiles :: Path -> Array Path
onlyFiles root = onlyFiles' (ls root)
  where
  onlyFiles' :: Array Path -> Array Path
  onlyFiles' paths =
    concatMap (\x -> if not isDirectory x then pure x else onlyFiles' (ls x)) paths

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ whereIs' $ allFiles path
  where
  whereIs' :: Array Path -> Array Path
  whereIs' paths = do
    pathname <- paths
    child <- ls pathname
    guard $ eq fileName $ fromMaybe "" $ last $ split (Pattern "/") $ filename child
    pure pathname

largestSmallest :: Path -> Array Path
largestSmallest path =
  let files = onlyFiles path
      maybeSizes = map size files
      maybeMax = foldl (outlier (>)) Nothing maybeSizes
      maybeMin = foldl (outlier (<)) Nothing maybeSizes
  in catMaybes $ map (findFileBySize files) $ nub $ [maybeMax, maybeMin]
  where
  outlier :: (Int -> Int -> Boolean) -> Maybe Int -> Maybe Int -> Maybe Int
  outlier criteria Nothing Nothing = Nothing
  outlier criteria (Just x) Nothing = Just x
  outlier criteria Nothing (Just x) = Just x
  outlier criteria (Just x1) (Just x2) = if criteria x1 x2 then Just x1 else Just x2
  findFileBySize :: Array Path -> Maybe Int -> Maybe Path
  findFileBySize files maybeSize = find (\file -> size file == maybeSize) files

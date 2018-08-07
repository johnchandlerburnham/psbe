module Exercises where

import Prelude

import Data.Array
import Data.Array.Partial (tail, head)
import Data.Foldable (product, and, foldl)
import Data.Tuple

import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)


length :: forall a. Array a -> Int
length arr = if null arr then 0
               else 1 + length (unsafePartial tail arr)

-- 4.4: (1)
isEven :: Int -> Boolean
isEven 1 = false
isEven 2 = true
isEven n = not $ isEven (n - 1)

-- 4.4: (2)
evens :: Array Int -> Int
evens arr = if null arr
              then 0
              else if (isEven $ (unsafePartial head) arr)
                  then 1 + (evens $ (unsafePartial tail) arr)
                  else (evens $ (unsafePartial tail) arr)
-- 4.7: (1)
squares :: Array Int -> Array Int
squares arr = map (\x -> x * x) arr

-- 4.7: (2)
removeNegatives :: Array Int -> Array Int
removeNegatives arr = filter (\x -> x >= 0) arr

-- 4.7: (3)
infix 7 filter as <$?>
removeNegatives' :: Array Int -> Array Int
removeNegatives' arr = (\x -> x >= 0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- 1 .. n
  pure [i, j]

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- 1 .. n
  guard $ i * j == n
  guard $ i /= 1 && j /= 1
  pure [i, j]

-- 4.11: (1)
isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 2

-- 4.11: (2)
cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure $ Tuple a b

-- 4.11: (3)
pythagoreanTriple :: Int -> Array (Array Int)
pythagoreanTriple n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a*a + b*b == c*c
  pure [a,b,c]

-- 4.11: (4)
-- This exercise is underspecified. The set of all arrays whose product is n is
-- infinite, because you can always add a 1 in any position. Instead, here's an
-- number sieve for getting a number's unique prime factorization.

factorizations :: Int -> Array Int
factorizations n = go n 2 []
  where
    go 1 _ fs = fs
    go n f fs
      | n `mod` f == 0 = go (n / f) 2 (f : fs)
      | otherwise = go n (f + 1) fs

testFact :: Int -> Boolean
testFact n = and $ zipWith (==) (2..n) $ map (product <<< factorizations) (2..n)

-- 4.15: (1)

and' :: Array Boolean -> Boolean
and' = foldl (&&) true

-- 4.16: (2)
-- the number of falses in the array is odd. The function (\x -> x == false) is
-- a lot like (\x -> x || false), which is a lot like (\x -> x * (-1)).

-- 4.16: (3)

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
               then count p (unsafePartial tail xs) + 1
               else count p (unsafePartial tail xs)

count' :: forall a. (a -> Boolean) -> Array a -> Int
count' p xs = go p xs 0
  where
    go p [] c = c
    go p xs c = if p (unsafePartial head xs)
                  then go p (unsafePartial tail xs) (c + 1)
                  else go p (unsafePartial tail xs) c

reverse' :: forall a. Array a -> Array a
reverse' xs = foldl (flip (:)) [] xs



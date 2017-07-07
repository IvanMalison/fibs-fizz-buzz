#!/usr/bin/env runhaskell
module FibsFizzBuzz where
import qualified Data.Map as M

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Int]
primes = sieve [2..] M.empty
  where
    sieve [] _ = []
    sieve (n:ns) iterators =
      case M.lookup n iterators of
        Nothing -> n : sieve ns (M.insert (n*n) [n] iterators)
        Just primesToIterate ->
          let reinsertIncrement i increment =
                M.insertWith (++) (n + increment) [increment] i
              withCurrentRemoved = M.delete n iterators
              updatedTable = foldl reinsertIncrement withCurrentRemoved primesToIterate
          in sieve ns updatedTable

-- XXX: This is pretty slow. I would probably use something like
-- https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test if
-- performance were a concern, but I won't pretend that I know how to implement
-- that off the top of my head.
isPrime :: Int -> Bool
isPrime n = recurse primes
  where recurse [] = recurse primes
        recurse (p:ps)
          | p > n = False
          | p == n = True
          | otherwise = recurse ps

toFizzBuzz :: Int -> String
toFizzBuzz i
  | i == 0 = show i
  | i `mod` 15 == 0 = "FizzBuzz"
  | isPrime i = "BuzzFizz"
  | i `mod` 5 == 0 = "Fizz"
  | i `mod` 3 == 0 = "Buzz"
  | otherwise = show i

fibsBuzz :: [String]
fibsBuzz = map toFizzBuzz fibs

fibsLength :: Int
fibsLength = 35

main :: IO ()
main = mapM_ putStrLn $ take fibsLength fibsBuzz

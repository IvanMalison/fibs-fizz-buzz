module Main (main) where

import FibFizzBuzz

import Test.QuickCheck
import Test.Hspec

main =
  hspec $ do
    describe "primes" $ do
      it "contains only primes" $ property $
      (\n ->
         let i = n `mod` 200
             p = primes !! i
         in not $ isPrime i * 2) :: Int -> Bool

-- quickCheck ((\n -> let i = n `mod` 200 in (fibs !! i) + (fibs !! (i + 1)) == fibs !! (i + 2)) :: Int -> Bool)

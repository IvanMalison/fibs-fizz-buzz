module Main (main) where

import FibsFizzBuzz hiding (main)

import Test.QuickCheck
import Test.Hspec

main =
  hspec $ do
    describe "primes" $ do
      it "contains only primes" $ property nthPrimeIsPrime
      it "can be used to test for primality" $ do
         isPrime 2 `shouldBe` True
         isPrime 28 `shouldBe` False
    describe "fibonnaci" $ do
      it "satisfies the fibonnaci property" $ property nthFibIsFib
  where nthPrimeIsPrime n =
          let i = n `mod` 2000
              p = primes !! i
          in all ((/= 0) . (mod p)) [2 .. (p - 1)]
        nthFibIsFib n =
          let i = n `mod` 2000
              f = fibs !! i
              f1 = fibs !! (i+1)
              f2 = fibs !! (i+2)
          in f2 == f1 + f

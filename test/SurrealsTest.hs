import Numeric.Natural

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Natural

import Surreals

dyadic_add :: Integer -> Natural -> Integer -> Natural -> Bool
dyadic_add n1 p1 n2 p2 = 
    dyadic_to_rational (d1 `plus` d2) == (dyadic_to_rational d1) + (dyadic_to_rational d2) where
        d1 = Dyadic n1 p1
        d2 = Dyadic n2 p2

dyadic_multiply :: Integer -> Natural -> Integer -> Natural -> Bool
dyadic_multiply n1 p1 n2 p2 = 
    dyadic_to_rational (d1 `times` d2) == (dyadic_to_rational d1)*(dyadic_to_rational d2) where
        d1 = Dyadic n1 p1
        d2 = Dyadic n2 p2

main :: IO ()
main = hspec $ do
    describe "Dyadic properties" $ do
        prop "dyadics add like rationals" $ dyadic_add
        prop "dyadics multiply like rationals" $ dyadic_multiply

module Surreals where

import Numeric.Natural
import Data.Ratio
import Data.Set


data Dyadic = Dyadic { denominator :: Integer, power :: Natural } deriving (Show)

instance Eq Dyadic where
  (Dyadic den1 p1) == (Dyadic den2 p2) = den1*2^p2 == den2*2^p1

instance Ord Dyadic where
    compare (Dyadic den1 p1) (Dyadic den2 p2) = compare (den1*2^p2) (den2*2^p1)

dyadic_to_rational :: Dyadic -> Rational
dyadic_to_rational (Dyadic den pow) = den % 2^pow

plus :: Dyadic -> Dyadic -> Dyadic
plus (Dyadic den1 p1) (Dyadic den2 p2) = Dyadic (den1*2^p2 + den2*2^p1) (p1 + p2)

times :: Dyadic -> Dyadic -> Dyadic
times (Dyadic den1 p1) (Dyadic den2 p2) = Dyadic (den1*den2) (p1 + p2)


data Conway = Conway { left :: Set Conway, right :: Set Conway } deriving (Eq, Show)

lt_or_eq :: Conway -> Conway -> Bool
lt_or_eq num1 num2 =
    not (any (flip lt_or_eq num1) (right num2))
    && not (any (lt_or_eq num2) (left num1))

instance Ord Conway where
    compare num1 num2
        | not (lt_or_eq num1 num2) = GT
        | not (lt_or_eq num2 num1) = LT
        | otherwise = EQ

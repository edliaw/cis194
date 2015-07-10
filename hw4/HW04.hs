{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P a) == (P b) = strip a == strip b where
        strip = dropWhileEnd (== 0)

-- Exercise 3 -----------------------------------------

showPoly :: (Num a, Eq a, Show a) => String -> a -> Integer -> String
showPoly _ 0 _ = ""
showPoly _ c 0 = show c
showPoly v c e = c' ++ e' where
    c' = if c == 1 then "" else show c
    e' = if e == 1 then v else v ++ "^" ++ show e

joinPoly :: [String] -> String
joinPoly [] = "0"
joinPoly p  = intercalate " + " p

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a) = joinPoly . filter (not . null) . reverse $ zipWith (showPoly "x") a [0..]

-- Exercise 4 -----------------------------------------

-- Padded zip
zipWithPad :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPad f _  b' as     []     = zipWith f as (repeat b')
zipWithPad f a' _  []     bs     = zipWith f (repeat a') bs
zipWithPad f a' b' (a:as) (b:bs) = f a b : zipWithPad f a' b' as bs

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ zipWithPad (+) 0 0 a b

-- Exercise 5 -----------------------------------------

foil :: Num a => [a] -> [a] -> [[a]]
foil a b = zipWith (++) zeros multiplicand where
    multiplicand = map (flip map b . (*)) a
    --multiplicand = zipWith (map . (*)) a $ repeat b
    zeros = iterate (0:) []

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum . map P $ foil a b

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate  (P a) = P $ map negate a
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P a) x' = sum $ zipWith (*) a (iterate (* x') 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = foldr (.) id $ replicate n deriv

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P a)  = P (zipWith (*) [1..] (tail a))

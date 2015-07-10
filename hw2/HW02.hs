{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = count (== True) $ zipWith (==) a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = zipWith (count . (==)) colors . repeat

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min (countColors a) (countColors b)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact nonexact where
    exact = exactMatches s g
    nonexact = matches s g - exact
        

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move g _ _) s = getMove s g == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
-- allCodes 1 = map (:[]) colors
-- allCodes n = [[a] ++ b | a <- colors, b <- allCodes (n-1)]
allCodes n = sequence $ replicate n colors

-- Exercise 7 -----------------------------------------

guesses :: Code -> [Code] -> [Move]
guesses _ [] = []
guesses s (g:gs) = m:(guesses s $ filterCodes m gs) where
    m = getMove s g

solve :: Code -> [Move]
solve s = guesses s . allCodes $ length s

-- Bonus ----------------------------------------------
fiveGuess :: Code -> [Move]
fiveGuess = undefined

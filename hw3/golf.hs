import Data.List

skip :: Int -> [a] -> [a]
skip n = map head . takeWhile (not . null) . iterate (drop n) . drop (n-1)
--skip n = map last  . takeWhile ((>= n) . length) . map (take n) . iterate (drop n)

skips :: [a] -> [[a]]
skips = takeWhile (not . null) . zipWith skip [1..] . repeat

localMaxima :: [Integer] -> [Integer]
localMaxima l = map (\(_,y,_) -> y)
                . filter (\(x,y,z) -> y >= x && y >= z)
                $ zip3 l (tail l) (drop 2 l)

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

counts :: [Integer] -> [Int]
counts = zipWith count [0..9] . repeat

fill :: [Int] -> [String]
fill ns = map (\n -> replicate (maximum ns - n) ' ' ++ replicate n '*') ns

histogram :: [Integer] -> String
histogram ns = unlines $ (transpose . fill $ counts ns) ++ ["==========\n0123456789"]

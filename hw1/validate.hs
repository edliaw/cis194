toDigits :: Integer -> [Integer]
toDigits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

doubleEveryOtherRight :: [Integer] -> [Integer]
doubleEveryOtherRight = reverse . doubleEveryOther . reverse

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id, (*2)])
-- e.g. [1,2,3] would expand to [id $ 1, (*2) $ 2, id $ 3]

zipCycle :: [a -> b] -> [a] -> [b]
zipCycle = zipWith id . cycle
-- doubleEveryOther = zipCycle [id, (*2)]

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

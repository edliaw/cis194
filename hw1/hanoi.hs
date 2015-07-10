
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a

nHanoi :: Integer -> [Peg] -> [Move]
nHanoi 1 (a:b:_) = [(a, b)]
nHanoi n (a:b:c_) = undefined

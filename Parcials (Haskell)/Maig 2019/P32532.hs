divisors :: Int -> [Int]
divisors n = filter (\x -> n `mod` x == 0) [1..n]

nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost n = and [nbDivisors x < nbDivisors n | x <- [1 .. n - 1]]
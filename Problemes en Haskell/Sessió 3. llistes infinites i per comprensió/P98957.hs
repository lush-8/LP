ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = 0 : concatMap (\x -> [x, -x]) (iterate (+1) 1)

triangulars :: [Integer]
triangulars = scanl (+) 0 (iterate (+1) 1)

factorials :: [Integer]
factorials = scanl (*) 1 (iterate (+1) 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = sieve (iterate (+1) 2)
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

hammings :: [Integer]
hammings = 1 : merge (map (*2) hammings) (merge (map (*3) hammings) (map (*5) hammings))
    where
        merge :: [Integer] -> [Integer] -> [Integer]
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys)
            | x < y = x : merge xs (y:ys)
            | x > y = y : merge (x:xs) ys
            | otherwise = x : merge xs ys

lookNsay :: [Integer]
lookNsay = iterate next 1
    where
        next :: Integer -> Integer
        next n = read (describe (show n))

        describe :: String -> String
        describe [] = []
        describe (c:cs) = show count ++ [c] ++ describe rest
            where
                run = c : takeWhile (== c) cs
                rest = dropWhile (== c) cs
                count = length run

tartaglia :: [[Integer]]
tartaglia = iterate nextRow [1]
    where
        nextRow row = zipWith (+) (0: row) (row ++ [0])
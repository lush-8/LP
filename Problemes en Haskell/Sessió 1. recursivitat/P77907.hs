absValue :: Int -> Int
absValue n 
    | n < 0 = -n
    | otherwise = n

power :: Int -> Int -> Int
power _ 0 = 1
power x p = x * power x (p - 1)

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | n `mod` 2 == 0 = False
    | otherwise = null [x | x <- [3,5..(floor (sqrt (fromIntegral n) :: Double))], n `mod` x == 0]

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2) 

quickFib :: Int -> Int
quickFib n = fib n 0 1
    where 
        fib 0 a b = a
        fib n a b = fib (n - 1) b (a + b)
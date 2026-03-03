myLength :: [Int] -> Int
myLength [] = 0
myLength (x:rest) = 1 + myLength rest

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:rest) = max x (myMaximum rest)

average :: [Int] -> Float
average xs = fromIntegral (mySum xs) / fromIntegral (myLength xs)
    where
        mySum [] = 0
        mySum (x:rest) = x + mySum rest

buildPalindrome :: [Int] -> [Int]
buildPalindrome xs = reverse xs ++ xs

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x:xs) y
    | x `elem` y = remove xs y
    | otherwise = x : remove xs y

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens (x:xs)
    | odd x = (x : odds, evens)
    | otherwise = (odds, x : evens)
    where
        (odds, evens) = oddsNevens xs

primeDivisors :: Int -> [Int]
primeDivisors n = [x | x <- [1..n], n `mod` x == 0, isPrime x]
    where
        isPrime :: Int -> Bool
        isPrime n
            | n <= 1 = False
            | n == 2 = True
            | n `mod` 2 == 0 = False
            | otherwise = null [x | x <- [3,5..(floor (sqrt (fromIntegral n) :: Double))], n `mod` x == 0]
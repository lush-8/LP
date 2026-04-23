df :: Int -> Int
df 0 = 1
df 1 = 1
df n = n * df (n - 2) 

sumd :: Int -> Int
sumd n = sum (decompose n)
    where
        decompose :: Int -> [Int]
        decompose 0 = []
        decompose n 
            | n < 10 = n : decompose (n `div` 10)
            | otherwise = (n `mod` 10) : decompose (n `div` 10)

dup :: [Int] -> [Int]
dup [] = []
dup (x:xs) = x : x : dup xs

pal :: String -> Bool
pal s = s == myReverse s []
    where
        myReverse :: String -> String -> String
        myReverse [] acc = acc
        myReverse (x:xs) acc = myReverse xs (x : acc)

apply2 :: (a -> a) -> a -> a
apply2 f x = f (f x)
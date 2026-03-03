countIf :: (Int -> Bool) -> [Int] -> Int
countIf p xs = length (filter p xs)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = map (\f -> map f xs) fs

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> map (\f -> f x) fs) xs

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int 
filterFoldl p f acc xs = foldl f acc (filter p xs)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] 
insert rel [] x = [x]
insert rel (y:ys) x
    | rel x y = x : y : ys
    | otherwise = y : insert rel ys x

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] 
insertionSort rel xs = foldl (\acc x -> insert rel acc x) [] xs
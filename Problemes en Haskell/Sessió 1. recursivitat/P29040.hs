insert :: [Int] -> Int -> [Int]
insert [] e = [e]
insert (x:xs) e 
    | e <= x = e : x : xs
    | otherwise = x : insert xs e

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) e
    | x == e = xs
    | otherwise = x : remove xs e

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs = m : ssort (remove xs m)
    where 
        m = minimum xs

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        mid = length xs `div` 2
        (left, right) = splitAt mid xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = (qsort smaller) ++ [p] ++ (qsort bigger)
    where
        smaller = [x | x <- xs, x < p]
        bigger = [x | x <- xs, x >= p]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (p:xs) = (genQsort smaller) ++ [p] ++ (genQsort bigger)
    where
        smaller = [x | x <- xs, x < p]
        bigger = [x | x <- xs, x >= p]
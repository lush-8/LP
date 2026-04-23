import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] _ = 0
degree ((x, y):rest) v
    | x == v = 1 + degree rest v
    | y == v = 1 + degree rest v
    | otherwise = degree rest v

degree' :: Eq a => [(a, a)] -> a -> Int
degree' edges v = foldl step 0 edges
    where
        step acc (x, y)
            | x == v = acc + 1
            | y == v = acc + 1
            | otherwise = acc

neighbors :: Ord a => [(a, a)] -> a -> [a]
neighbors edges v = sort (foldr step [] edges)
    where
        step (x, y) acc
            | x == v = y : acc
            | y == v = x : acc
            | otherwise = acc
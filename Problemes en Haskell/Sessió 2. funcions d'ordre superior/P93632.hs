eql :: [Int] -> [Int] -> Bool
eql xs ys = xs == ys

prod :: [Int] -> Int
prod xs = product xs

prodOfEvens :: [Int] -> Int
prodOfEvens xs = product (filter even xs) 

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum (zipWith (*) xs ys)
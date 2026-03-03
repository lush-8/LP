flatten :: [[Int]] -> [Int]
flatten xs = concat xs

myLength :: String -> Int
myLength xs = length xs

myReverse :: [Int] -> [Int]
myReverse xs = reverse xs

countIn :: [[Int]] -> Int -> [Int]
countIn xs e = map (\sub -> length (filter (== e) sub)) xs

firstWord :: String -> String
firstWord xs = takeWhile (/= ' ') string
    where
        string = dropWhile (== ' ') xs
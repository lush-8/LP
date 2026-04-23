bell :: [[Int]]
bell = iterate next [1]
    where
        next :: [Int] -> [Int]
        next list = (last list) : (build list (last list))
        
        build :: [Int] -> Int -> [Int]
        build [] _ = []
        build (x:xs) elem = (elem + x) : build xs (elem + x)
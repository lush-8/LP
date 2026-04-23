main :: IO ()
main = do
    contingut <- getContents
    let enters = map read (words contingut) :: [Int]
    let resultat = calcula enters
    print resultat

calcula :: [Int] -> (Int, Int)
calcula (x:xs) = foldl actualitza (x, x) xs
    where
        actualitza (maxim, minim) n = (max maxim n, min minim n)
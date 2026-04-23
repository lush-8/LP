import Text.Read (readMaybe)

main :: IO ()
main = getContents >>= print . process
    where
        process :: String -> Maybe (Int, Int)
        process input = traverse readMaybe (words input) >>= calcula

calcula :: [Int] -> Maybe (Int, Int)
calcula [] = Nothing
calcula (x:xs) = pure (foldl actualitza (x, x) xs)
    where
        actualitza (maxim, minim) n = (max maxim n, min minim n)
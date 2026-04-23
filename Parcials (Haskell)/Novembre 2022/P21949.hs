main :: IO ()
main = do
    contingut <- getContents
    mapM_ putStrLn (parse (words contingut))
        where
            parse :: [String] -> [String]
            parse (n : orig : dest : aux : _) = hanoi (read n) orig dest aux
            parse _ = []

hanoi :: Int -> String -> String -> String -> [String]
hanoi 0 _ _ _ = []
hanoi n orig dest aux =
    hanoi (n - 1) orig aux dest ++
    [orig ++ " -> " ++ dest] ++
    hanoi (n - 1) aux dest orig
seqtm :: [String]
seqtm = iterate build "0" 
    where
        build :: String -> String
        build list = list ++ complement list
            where
                complement :: String -> String
                complement [] = ""
                complement (x:xs)  
                    | x == '1' = '0' : complement xs
                    | otherwise = '1' : complement xs 
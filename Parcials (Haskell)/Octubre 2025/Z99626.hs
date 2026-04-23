data Node a = Node a [a]
data Graf a = Graf [Node a] 

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

instance Show a => Show (Node a) where
    show (Node v []) = show v
    show (Node v adj) = show v ++ " : " ++ intercalate ", " (map show adj)

instance Show a => Show (Graf a) where
    show (Graf nodes) = intercalate "\n" (map show nodes)

cami :: Eq a => Graf a -> (a, a) -> Bool
cami (Graf nodes) (orig, dest) = dfs [orig] []
    where
        dfs [] _ = False
        dfs (x:xs) vis
            | x == dest = True
            | x `elem` vis = dfs xs vis
            | otherwise = dfs (adj x ++ xs) (x : vis)

        adj v = find v nodes

        find _ [] = []
        find v (Node n adj : ns)
            | v == n = adj
            | otherwise = find v ns

main :: IO()
main = do
    contingut <- getContents
    process contingut
        where
            process "" = return ()	  
            process text = print (cami graf (orig, dest))
                where
                    linies = lines text

                    first = words (head linies)

                    orig = head first

                    dest = first !! 1
                    
                    graf = Graf (map build (tail linies))

                    build input = Node vertex llistaAdj
                        where
                            elements = words input
                            vertex = head elements
                            llistaAdj = tail elements
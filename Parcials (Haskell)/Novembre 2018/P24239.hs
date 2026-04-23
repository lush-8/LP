val :: Char -> Int
val 'I' = 1
val 'V' = 5
val 'X' = 10
val 'L' = 50
val 'C' = 100
val 'D' = 500
val 'M' = 1000

roman2int :: String -> Int
roman2int [] = 0 
roman2int [c] = val c
roman2int (c1:c2:cs)
    | val c1 < val c2 = -(val c1) + roman2int (c2:cs)
    | otherwise = val c1 + roman2int (c2:cs)

roman2int' :: String -> Int
roman2int' s = fst (foldr step (0, 0) s)
    where
        step c (total, max)
            | val c < max = (total - val c, max)
            | otherwise = (total + val c, val c)

arrels :: Float -> [Float]
arrels x = iterate step x
    where
        step prev = 0.5 * (prev + x / prev)

arrel :: Float -> Float -> Float
arrel x eps = find (arrels x)
    where
        find (t1:t2:ts)
            | abs (t1 - t2) <= eps = t2
            | otherwise = find (t2:ts)
        find _ = 0

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance Show a => Show (LTree a) where
    show (Leaf x) = "{" ++ show x ++ "}"
    show (Node l r) = "<" ++ show l ++ "," ++ show r ++ ">"

build :: [a] -> LTree a
build [x] = Leaf x
build xs = Node (build l) (build r)
    where
        half = (length xs + 1) `div` 2

        (l, r) = splitAt half xs

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a, b))
zipLTrees (Leaf x) (Leaf y) = do
    return (Leaf (x, y))
zipLTrees (Node l1 r1) (Node l2 r2) = do
    left <- zipLTrees l1 l2
    right <- zipLTrees r1 r2
    return (Node left right)
zipLTrees _ _ = Nothing
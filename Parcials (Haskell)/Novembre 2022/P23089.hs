myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f seed = process (f seed)
    where
        process Nothing = []
        process (Just (element, newSeed)) = element : myUnfoldr f newSeed

myReplicate :: a -> Int -> [a]
myReplicate x n = myUnfoldr step n
    where
        step count
            | count <= 0 = Nothing
            | otherwise = Just (x, count - 1)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = myUnfoldr step x
    where
        step val = Just (val, f val)

myMap :: (a -> b) -> [a] -> [b]
myMap f list = myUnfoldr step list 
    where 
        step [] = Nothing
        step (y:ys) = Just (f y, ys)

data Bst a = Empty | Node a (Bst a) (Bst a) 

instance Show a => Show (Bst a) where
    show Empty = "."
    show (Node y l r) = "(" ++ show y ++ " " ++ show l ++ " " ++ show r ++ ")"

add :: Ord a => a -> (Bst a) -> (Bst a)

add x Empty = Node x Empty Empty 
add x (Node y l r)
    | x < y          = Node y (add x l) r
    | x > y          = Node y l (add x r)
    | otherwise = Node y l r

adder :: Ord a => (Bst a, [a]) -> Maybe (Bst a, (Bst a, [a]))
adder (_, []) = Nothing
adder (arbre, x:xs) = Just (nouArbre, (nouArbre, xs))
    where
        nouArbre = add x arbre
data STree a = Nil | Node Int a (STree a) (STree a) deriving Show

size :: STree a -> Int
size Nil = 0 
size (Node s _ _ _) = s

isOk :: STree a -> Bool
isOk Nil = True
isOk (Node s _ l r) = s == size l + size r + 1 && isOk l && isOk r

nthElement :: STree a -> Int -> Maybe a
nthElement Nil _ = Nothing
nthElement (Node _ val l r) n
    | n <= 0 = Nothing
    | n == leftSize + 1 = Just val
    | n <= leftSize = nthElement l n
    | otherwise = nthElement r (n - leftSize - 1)
    where
        leftSize = size l

mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements f tree indices = map process indices
    where
        process n = fmap f (nthElement tree n)

instance Functor STree where
    fmap _ Nil = Nil
    fmap f (Node s val l r) = Node s (f val) (fmap f l) (fmap f r)
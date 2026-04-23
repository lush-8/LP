data Tree a = Empty | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show Empty = "()"
    show (Node val l r) = "(" ++ show l ++ "," ++ show val ++ "," ++ show r ++ ")"

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

doubleT :: Num a => Tree a -> Tree a
doubleT = fmap (*2)

data Forest a = Forest [Tree a] deriving (Show)

instance Functor Forest where
    fmap f (Forest forest) = Forest (map (fmap f) forest)

doubleF :: Num a => Forest a -> Forest a
doubleF = fmap (*2)
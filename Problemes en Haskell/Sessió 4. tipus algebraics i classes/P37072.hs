data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a leftA rightA) (Node b leftB rightB) = a == b && equal leftA leftB && equal rightA rightB

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a leftA rightA) (Node b leftB rightB) = a == b && check
    where
        check
            | isomorphic leftA leftB && isomorphic rightA rightB = True
            | isomorphic leftA rightB && isomorphic rightA leftB = True
            | otherwise = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a left right) = a : (preOrder left ++ preOrder right)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a left right) = inOrder left ++ (a : inOrder right)

breadthFirst :: Tree a -> [a]
breadthFirst tree = bfs [tree]
    where
        bfs :: [Tree a] -> [a]
        bfs [] = []
        bfs (Empty : ts) = bfs ts
        bfs (Node a left right : ts) = a : bfs (ts ++ [left, right])

build :: Eq a => [a] -> [a] -> Tree a
build [] _ = Empty
build _ [] = Empty
build (root:preOrder) inOrder = Node root (build preEsq inEsq) (build preDret inDret)
    where
        inEsq = takeWhile (/= root) inOrder

        inDret = drop 1 (dropWhile (/= root) inOrder)

        midaEsq = length inEsq

        preEsq = take midaEsq preOrder
        
        preDret = drop midaEsq preOrder

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ t Empty = t
overlap _ Empty t = t
overlap f (Node a leftA rightA) (Node b leftB rightB) = Node (f a b) (overlap f leftA leftB) (overlap f rightA rightB)
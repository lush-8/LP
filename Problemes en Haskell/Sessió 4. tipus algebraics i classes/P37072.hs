data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int

height :: Tree a -> Int

equal :: Eq a => Tree a -> Tree a -> Bool

isomorphic :: Eq a => Tree a -> Tree a -> Bool

preOrder :: Tree a -> [a]

postOrder :: Tree a -> [a]

inOrder :: Tree a -> [a]

breadthFirst :: Tree a -> [a]

build :: Eq a => [a] -> [a] -> Tree a

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
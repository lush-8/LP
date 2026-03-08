data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue xs ys) = Queue xs (x:ys)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue (x:xs) ys) = Queue xs ys
pop (Queue [] ys) = pop (Queue (reverse ys) [])

top :: Queue a -> a
top (Queue (x:_) _) = x
top (Queue [] ys) = head (reverse ys)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a)
    where
        (Queue xs1 ys1) == (Queue xs2 ys2) = (xs1 ++ reverse ys1) == (xs2 ++ reverse ys2)
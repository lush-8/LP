data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
push :: a -> Queue a -> Queue a
pop :: Queue a -> Queue a
top :: Queue a -> a
empty :: Queue a -> Bool

instance Eq a => Eq (Queue a)
    where
        ...
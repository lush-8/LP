data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push elem (Queue a b) = Queue a (elem:b)

pop :: Queue a -> Queue a
pop (Queue [] b) = pop (Queue (reverse b) [])
pop (Queue (_:xs) b) = Queue xs b

top :: Queue a -> a
top (Queue [] b) = last b
top (Queue a _) = head a

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

qToList :: Queue a -> [a]
qToList (Queue front back) = front ++ reverse back

instance Eq a => Eq (Queue a) where
    q1 == q2 = qToList q1 == qToList q2

instance Functor Queue where
    fmap f (Queue front back) = Queue (fmap f front) (fmap f back)

translation :: Num b => b -> Queue b -> Queue b
translation n t = fmap (+n) t

instance Applicative Queue where
    pure x  = Queue [x] []
    qf <*> qx = Queue (qToList qf <*> qToList qx) []

merge :: Queue a -> Queue a -> Queue a
merge (Queue a1 b1) (Queue a2 b2) = Queue (a1 ++ reverse b1 ++ a2) b2

flatten :: [Queue a] -> Queue a
flatten [] = create
flatten [q] = q
flatten (q:qs) = merge q (flatten qs)

instance Monad Queue where
    return x = Queue [x] []
    q >>= f = flatten [f x | x <- qToList q]

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter p q = do
    x <- q

    if p x
        then return x
        else create
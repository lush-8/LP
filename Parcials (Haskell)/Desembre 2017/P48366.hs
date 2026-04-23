eval1 :: String -> Int
eval1 s = evaluate [] (words s)
    where
        evaluate [res] [] = res
        evaluate (y:x:stack) ("+":tokens) = evaluate ((x + y) : stack) tokens
        evaluate (y:x:stack) ("-":tokens) = evaluate ((x - y) : stack) tokens
        evaluate (y:x:stack) ("*":tokens) = evaluate ((x * y) : stack) tokens
        evaluate (y:x:stack) ("/":tokens) = evaluate ((x `div` y) : stack) tokens
        evaluate stack (n:tokens) = evaluate (read n : stack) tokens

eval2 :: String -> Int
eval2 s = head (foldl process [] (words s))
    where
        process (y:x:stack) "+" = (x + y) : stack
        process (y:x:stack) "-" = (x - y) : stack
        process (y:x:stack) "*" = (x * y) : stack
        process (y:x:stack) "/" = (x `div` y) : stack
        process stack n = read n : stack

fsmap :: a -> [a -> a] -> a
fsmap x fs = foldl (\current func -> func current) x fs

divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x = processBaseCase (base x)
    where
        processBaseCase (Just trivial) = trivial
        processBaseCase Nothing = processDivide (divide x)

        processDivide (sub1, sub2) = conquer x (sub1, sub2) (solve sub1, solve sub2)

        solve subProblem = divideNconquer base divide conquer subProblem

quickSort :: [Int] -> [Int]
quickSort = divideNconquer base divide conquer
    where
        base [] = Just []
        base [x] = Just [x]
        base _ = Nothing

        divide (pivot:tail) = (filter (< pivot) tail, filter (>= pivot) tail)

        conquer (pivot:_) _ (lesserOrdered, greaterOrdered) = lesserOrdered ++ [pivot] ++ greaterOrdered

data Racional = R Integer Integer

racional :: Integer -> Integer -> Racional
racional n d = R (n `div` mcd) (d `div` mcd)
    where
        mcd = gcd n d

numerador :: Racional -> Integer
numerador (R n _) = n

denominador :: Racional -> Integer
denominador (R _ d) = d

instance Eq Racional where
    (R n1 d1) == (R n2 d2) = n1 == n2 && d1 == d2

instance Show Racional where
    show (R n d) = show n ++ "/" ++ show d

data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where 
        recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

arbreCalkinWilf :: Racional -> Tree Racional
arbreCalkinWilf r = Node r left right
    where
        a = numerador r

        b = denominador r

        left = arbreCalkinWilf (racional a (a + b))

        right = arbreCalkinWilf (racional (a + b) b)

racionals :: [Racional]
racionals = recXnivells (arbreCalkinWilf (racional 1 1))